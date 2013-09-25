Definitional Interpreters
=========================

- Reynolds: “An important and frequently used method of defining a programming language is to give an interpreter for the language which is written in a second, hopefully better understood language.”

- Clarity over efficiency
- Executable specification

Other Language Artifacts?
=========================

We want other artifacts, too:

- Fast interpreters
- Static analyzers
- Optimizing compilers

We also want a variety of languages. N x M problem?


Contributions
=============

- Construct analyzers and program transformers 
	- by refining the implementation of an interpreter
- The initial interpreter is a 'definitional abstract interpreter':
	- definitional: no distracting detail beyond the essence of the language (at the coarsest granularity)
	- abstract: parametric in the semantic domain, covers both precise and approximate values
- Pieces of infrastructure can be shared across implementations


Contributions
=============

It is known that staging an interpreter yields a (simple) compiler.

We show how to build a variety of program transformers, analyzers, 
and optimizing compilers using essentially the same mechanism.


Roadmap
=======
Intro:

- A small while language
- Interpreter, compiler, static analyzer

Recap finally tagless/polymorphic embedding:

- "Definitional abstract interpreters"


A Small Language
======================

	abstract class Exp                                     // e ::= 
	case class Lit(c: Int) extends Exp                     //   c
	case class Plus(a: Exp, b: Exp) extends Exp            //   e + e
	case class Times(a: Exp, b: Exp) extends Exp           //   e * e
	case class Ref(a: String) extends Exp                  //   x

	abstract class Stm                                     // s ::= 
	case class Assign(a: String, b: Exp) extends Stm       //   x := e
	case class Block(as: List[Stm]) extends Stm            //   { s* }
	case class If(c: Exp, a: Stm, b: Stm) extends Stm      //   if (e) s else s
	case class While(c: Exp, b: Stm) extends Stm           //   while (e) b

	case class Prog(a: String, b: List[Stm], c: Exp)       // p ::= x := input; s*; return e

Definitional Interpreter
========================

	val store = new HashMap[String,Int]
	def eval(e: Exp): Int = e match {
		case Lit(c)      => c
		case Plus(a,b)   => eval(a) + eval(b)
		case Times(a,b)  => eval(a) * eval(b)
		case Ref(a)      => assert(store.contains(a)); store(a)
    }
    def exec(s: Stm): Unit = s match {
    	case Assign(a,b) => store(a) = eval(b)
    	case Block(as)   => as.foreach(exec)
    	case If(c,a,b)   => if (eval(c) != 0) exec(a) else exec(b)
    	case While(c,b)  => while (eval(c) != 0) exec(b)
    }
    def run(p: Program, x: Int): Int = p match {
    	case Prog(a,b,c) => store.clear; store(a) = x; exec(b); eval(c)
    }

Other Language Artifacts?
=========================

We want other artifacts, too:

- Fast interpreters
- Static analyzers
- Optimizing compilers


A Simple Compiler
=================
Map the interpreter to code describing each of its actions:

	def eval(e: Exp): String = e match {
		case Lit(c)      => s"$c"
		case Plus(a,b)   => s"(${eval(a)}" + ${eval(b)})"
		case Times(a,b)  => s"(${eval(a)}" * ${eval(b)})"
		case Ref(a)      => s"{assert(store.contains($a)); store($a)}"
    }
    def exec(s: Stm): Unit = s match {
    	case Assign(a,b) => s"store($a) = ${eval(b)}"
    	case Block(as)   => s"{ ${as.map(exec).mkString(";")} }"
    	case If(c,a,b)   => s"if (${eval(c)} != 0) ${exec(a)} else ${exec(b)}"
    	case While(c,b)  => s"while (${eval(c)} != 0) ${exec(b)}"
    }
    def run(p: Program, x: Int): Int = p match {
    	case Prog(a,b,c) => s"def main(x:Int) = { "+
	    	s"val store = new HashMap[String,Int]; store($a) = x; " +
	    	s"${exec(b)}; ${eval(c) }}""
    }

Removing dispatch overhead, but no

A Simple Static Analyzer
========================

Ensure that a program does not reference uninitialized variables:

	val store = new HashSet[String]
	def eval(e: Exp): Unit = e match {
		case Lit(c)      => 
		case Plus(a,b)   => eval(a); eval(b)
		case Times(a,b)  => eval(a); eval(b)
		case Ref(a)      => assert(store.contains(a))
    }
    def exec(s: Stm): Unit = s match {
    	case Assign(a,b) => eval(b); store += a
    	case Block(as)   => as.foreach(exec)
    	case If(c,a,b)   => eval(c); 
    						 val save = store; 
    						 exec(a)
  						 	 val onlyA = store; store = save;
  						 	 exec(b)
  						 	 val onlyB = store;
  						 	 store = onlyA intersection onlyB
    	case While(c,b)  => eval(c); val save = store; exec(b); store = save
    }
    def run(p: Program): Unit = p match {
    	case Prog(a,b,c) => store.clear; store(a) = x; exec(b); eval(c)
    }

Economy of Concepts?
====================

Compiler and analyzer were derived more or less mechanically from interpreter. 
In hindsight, there are obvious similarities. But we still need to:

- implement artifacts one by one
	- we would like to compose analysis + compiler = optimizing compiler
- prove that they are sound w.r.t. to the interpreter!

Can we do better?


Definitional Abstract Interpreter
=================================

- When something looks common:
	- add a layer of abstraction to factor out commonalities
- "abstract" in abstract interpreter can mean different things:
	- abstract as in approximate (Cousot-style abstract interpretation)
	- abstract as in parametrism, representation independence
	- we mostly mean the latter (but will get back to the former)


Extreme Case: Identity transform
================================

We want to peform various program transformations. To be general, 
we need to be able to also implement the most basic one, 
the identity transform.

	def eval(e: Exp): Exp = e match {
		case Lit(c)      => Lit(c)
		case Plus(a,b)   => Plus(eval(a), eval(b))
		case Times(a,b)  => Time(eval(a), eval(b))
		case Ref(a)      => Ref(a)
    }
    def exec(s: Stm): Stm = s match {
    	case Assign(a,b) => Assign(a,eval(b))
    	case Block(as)   => Block(as.map(exec))
    	case If(c,a,b)   => If(eval(c),exec(a),exec(b))
    	case While(c,b)  => While(eval(c),exec(b))
    }
    def run(p: Prog): Prog = p match {
    	case Prog(a,b,c) => Prog(a,exec(b),eval(c))
    }

Abstract Types and Abstract Operations
======================================

Claim: if we can span the identity transform and concrete execution in one
abstract model we can do everything else, too.

We switch from concrete values `Int` to abstract values `Val` and
abstract over the `Control` type of statements, too.
We define each language operation as an abstract method.

	def eval(e: Exp): Val = e match {
		case Lit(c)      => lit(c)
		case Plus(a,b)   => plus(eval(a), eval(b))
		case Times(a,b)  => times(eval(a), eval(b))
		case Ref(a)      => ref(a)
    }
    def exec(s: Stm): Control = s match {
    	case Assign(a,b) => assign(a,eval(b))
    	case Block(as)   => block(as.map(exec))
    	case If(c,a,b)   => if_(eval(c),exec(a),exec(b))
    	case While(c,b)  => while_(eval(c),exec(b))
    }
    def run(p: Prog): Result = p match {
    	case Prog(a,b,c) => prog(a,exec(b),eval(c))
    }

Abstract Types and Abstract Operations
======================================

Interface:

	type Val
	type Control
	type Result

	def lit(c:Int): Val
	def plus(a:Val,b:Val): Val
	def times(a:Val,b:Val): Val
	def ref(a:String): Val

	def assign(a:String,b:Val): Control
	def block(a:List[Control]): Control
	def if_(c:Val,a: =>Control,b: =>Control): Control
	def while_(c: =>Val,b: =>Control): Control

	def prog(a:String,b: =>Control,c: =>Val): Result

Note that we use by-name parameters (`=>` types) to reflect the evaluation order.

For nicer surface syntax, we can use language virtualization (use `+` instead of `plus` etc).


Recovering Concrete Interpretation
==================================
	type Val = Int
	type Control = Unit
	type Result = Int => Int

	val store = new HashMap[String,Int]

	def lit(c:Int)         = c
	def plus(a:Val,b:Val)  = a + b
	def times(a:Val,b:Val) = a * b
	def ref(a:String)      = { assert(store.contains(a)); store(a) }

	def assign(a:String,b:Val)               = store(a) = b
	def block(a:List[Control])               = ()
	def if_(c:Val,a: =>Control,b: =>Control) = if (c) a else b
	def while_(c: =>Val,b: =>Control)        = while (c) b

	def prog(a:String,b: =>Control,c: =>Val) = { x: Int =>
	  store.clear; store(a) = x; b; c
	}

Recovering Compilation and Analysis
===================================

In the same manner.

What have we gained? So far not all that much. We still have almost
separate implementations.

We now go ahead and recover more commonalities.


Abstracting State/Control
=========================

Key: need nondeterminism. Execute both branches of conditionals.
Fixpoint iteration for while loops.

	type State
	def orElse(a:State,b:State): State
	def eq(a:State,b:State): Boolean
	
	def reify(x: =>Control): State=>State
	def reflect(x: State=>State): Control
	
	def if_(c:Val,a: =>Control,b: =>Control) = reflect { s0: State =>
		val sA = reify(a)(s0)
		val sB = reify(b)(s0)
		orElse(sA,sB)
	}
	def while_(c: =>Val,b: =>Control) = reflect { s0: State =>
		
	
		if (c) { b; while (c) b } else { } // XXX
	}
	
Abstract Interpretation
=======================

Constant folding, sign analysis


Optimizing Compilation
======================

Combine analysis and translation




Program Transformation
======================

So far, our compiler did not do much to speed up programs.

Now we show:

	- layering transforms
	- ANF transform
	- transforming structured control flow to jumps
	- compile analysis and transformation --> optimizing compiler
	
Layering Transforms
===================

	val next: Symantics
	def pre(e:Val): next.Val
	def post(e:next.Val): Val

	def plus(a:Val,b:Val)  = post(next.plus(pre(a),pre(b)))
	def times(a:Val,b:Val) = reflect(next.times(ref(a),ref(b)))

Instead of explicit `pre` and `post` operations we can use implicit conversions in Scala.
We can also use mixin composition and `super` calls instead of explicit `next` calls.

	
ANF Transform
=============
	type Val = Sym
	def emit(e:Stm) = ...

	val next: Symantics
	def reflect(e:next.Val): Val = { val x = fresh; emit(assign(x,e)); ref(x) }

	def plus(a:Val,b:Val)  = reflect(next.plus(ref(a),ref(b)))
	def times(a:Val,b:Val) = reflect(next.times(ref(a),ref(b)))


Structured Control Flow to Jumps
================================

	type Label
	def goto(k:Label): Ans
	def ifgoto(c:Val,a:Label,b:Label):Ans
	def define(a: =>Ans): Label
	def shift(f: Label => Ans): Control

	def if_(c:Val,a: =>Control,b: =>Control) = shift { k =>
		val u = define { a; goto(k) }
		val v = define { b; goto(k) }		
		ifgoto(c,u,v)
	}
	def while_(c: =>Val,b: =>Control) = shift { k =>
		val u = define { ifgoto(c,v,k) }
		val v = define { b; goto(u) }
		goto(u)
	}

package intp.direct

import scala.collection.mutable.HashMap
import collection.mutable

trait Syntax {

  // language syntax

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

  case class Prog(a: String, b: Stm, c: Exp)             // p ::= x := input; s; return e

}

trait Examples extends Syntax {

  // example programs

  // fac(n) = n * fac(n-1)
  val fac = Prog("n",Block(List(                         // n := input
    Assign("r",Lit(1)),                                  // r := 1
    While(Ref("n"),Block(List(                           // while (n) {
      Assign("r",Times(Ref("r"),Ref("n"))),              //   r := r * n
      Assign("n",Plus(Ref("n"),Lit(-1)))                 //   n := n - 1
    ))))),                                               // }
    Ref("r")                                             // return r
  )

  // Simple looping program
  val loop = Prog("n",Block(List(                        // n := input
    Assign("r",Lit(5)),                                  // r := 5
    Assign("s",Lit(3)),                                  // s := 3
    While(Ref("n"),Block(List(                           // while (n) {
      If(Ref("s"),                                       //   if (s)
        Block(List(Assign("r",Times(Ref("r"),Ref("s"))), //   then r := r * s
              Assign("s",Plus(Ref("s"),Lit(-1))))),      //        s := s - 1
        Block(List()))                                   //   else skip
    ))))),                                               // }
    Ref("r")                                             // return r
  )

}

trait DirectInterpreter extends Syntax {

  // definitional interpreter

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
  def run(p: Prog)(x: Int): Int = p match {
    case Prog(a,b,c) => store.clear; store(a) = x; exec(b); eval(c)
  }

}


object TestDirectInterpreter extends DirectInterpreter with Examples {

  // tests

  def main(args: Array[String]): Unit = {
    assert(run(fac)(4) == 24)
  }
}


trait DirectCompiler extends Syntax {

  // simple compiler

  def eval(e: Exp): String = e match {
    case Lit(c)      => s"$c"
    case Plus(a,b)   => s"(${ eval(a) } + ${ eval(b) }"
    case Times(a,b)  => s"(${ eval(a) } * ${ eval(b) }"
    case Ref(a)      => s"{ assert(store.contains($a)); store($a) }"
  }
  def exec(s: Stm): String = s match {
    case Assign(a,b) => s"store($a) = ${ eval(b) }"
    case Block(as)   => s"{\n${ as.map(exec).mkString("\n") }\n}"
    case If(c,a,b)   => s"if (${ eval(c) } != 0) ${ exec(a) } else ${ exec(b) }"
    case While(c,b)  => s"while (${ eval(c) } != 0) ${ exec(b) }"
  }
  def run(p: Prog): String = p match {
    case Prog(a,b,c) => s"def main($a: Int): Int = {\nval store = new HashMap[String,Int]\nstore($a) = x\n${ exec(b) }\n${ eval(c) }"
  }

}


object TestDirectCompiler extends DirectCompiler with Examples {

  // tests

  def main(args: Array[String]): Unit = {
    println(run(fac))
    assert(run(fac) ==
"""def main(n: Int): Int = {
val store = new HashMap[String,Int]
store(n) = x
{
store(r) = 1
while ({ assert(store.contains(n)); store(n) } != 0) {
store(r) = ({ assert(store.contains(r)); store(r) } * { assert(store.contains(n)); store(n) }
store(n) = ({ assert(store.contains(n)); store(n) } + -1
}
}
{ assert(store.contains(r)); store(r) }""")
  }

}

trait IdentityTransform extends Syntax {

  // identity transform

  def eval(e: Exp): Exp = e match {
    case Lit(c)      => Lit(c)
    case Plus(a,b)   => Plus(eval(a),eval(b))
    case Times(a,b)  => Times(eval(a),eval(b))
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

}

object TestIdentityTransform extends IdentityTransform with Examples {

  // tests

  def main(args: Array[String]): Unit = {
    println(run(fac))
    assert(run(fac) == fac)
  }

}


trait AbstractSyntax {

  // language syntax

  type Val
  def lit(c: Int): Val
  def plus(a: Val, b: Val): Val
  def times(a: Val, b: Val): Val
  def ref(a: String): Val

  type Control
  def assign(a: String, b: Val): Control
  def block(as: List[() => Control]): Control
  def if_(c: Val, a: => Control, b: => Control): Control
  def while_(c: => Val, b: => Control): Control

  type Program
  def prog(a: String, b: =>Control, c: =>Val): Program

}

trait AbstractInterpreter extends Syntax with AbstractSyntax {

  // definitional abstract interpreter

  def eval(e: Exp): Val = e match {
    case Lit(c)      => lit(c)
    case Plus(a,b)   => plus(eval(a),eval(b))
    case Times(a,b)  => times(eval(a),eval(b))
    case Ref(a)      => ref(a)
  }
  def exec(s: Stm): Control = s match {
    case Assign(a,b) => assign(a,eval(b))
    case Block(as)   => block(as.map(st => (() => exec(st))))
    case If(c,a,b)   => if_(eval(c),exec(a),exec(b))
    case While(c,b)  => while_(eval(c),exec(b))
  }
  def run(p: Prog): Program = p match {
    case Prog(a,b,c) => prog(a,exec(b),eval(c))
  }

}

trait AbstractDirectInterpreter extends AbstractSyntax {

  val store = new HashMap[String,Int] // let's hide it from successors (made it public again for tracing)

  type Val = Int
  def lit(c: Int): Val = c
  def plus(a: Val, b: Val): Val = a + b
  def times(a: Val, b: Val): Val = a * b
  def ref(a: String): Val = { assert(store.contains(a)); store(a) }

  type Control = Unit
  def assign(a: String, b: Int): Unit = store(a) = b
  def block(as: List[() => Unit]): Unit = as.map(_.apply())
  def if_(c: Val, a: => Unit, b: => Unit): Unit = if (c != 0) a else b
  def while_(c: => Int, b: => Unit): Unit = while (c != 0) b

  type Program = Int => Int
  def prog(a: String, b: =>Unit, c: =>Int) = { x:Int =>
    store.clear; store(a) = x; b; c
  }
}

object TestAbstractDirectInterpreter extends AbstractInterpreter with AbstractDirectInterpreter with Examples {

  // tests

  def main(args: Array[String]): Unit = {
    assert(run(fac)(4) == 24)
  }

}

trait AbstractLabeledSyntax extends AbstractSyntax {
  type Label
  def label: Label
}

trait AbstractLabeling extends AbstractLabeledSyntax {

  abstract class Label
  case object Root extends Label
  case class InBlock(n: Int, up: Label) extends Label
  case class InThen(up: Label) extends Label
  case class InElse(up: Label) extends Label
  case class InLoop(up: Label) extends Label
  //case class InAsgn(x: String, up: Layer) extends Layer // assignments are atomic stms, so we can't rely be in one

  var label: Label = Root
  def inLabel[A](f: Label => Label)(b: => A) = {
    val save = label
    label = f(label)
    try b finally label = save
  }

  abstract override def block(as: List[() => Control]): Control =
    super.block(as.zipWithIndex map { case (f,i) => () => inLabel(InBlock(i,_))(f()) })
  abstract override def if_(c: Val, a: => Control, b: => Control): Control =
    super.if_(c, inLabel(InThen)(a), inLabel(InElse)(b))
  abstract override def while_(c: => Val, b: => Control): Control =
    super.while_(c, inLabel(InLoop)(b))

  abstract override def prog(a: String, b: =>Control, c: =>Val): Program =
    super.prog(a, inLabel(_ => Root)(b), c)

}

trait AbstractTracing extends AbstractLabeling {

  def store: Any

  override def inLabel[A](f: Label => Label)(b: => A) = 
    super.inLabel(f) { println(s"pp: $label".padTo(50,' ') + s"state: $store"); b }

}

object TestAbstractTracingInterpreter extends AbstractInterpreter with AbstractDirectInterpreter with AbstractTracing with Examples {

  def main(args: Array[String]): Unit = {
    assert(run(fac)(4) == 24)
  }

}

/*object TestAbstractTracingInterpreter2 extends AbstractInterpreter with AbstractTracingInterpreter with Examples {

  def main(args: Array[String]): Unit = {
    println(run(loop)(4))
  }

}*/

trait AbstractCollectingInterpreter extends AbstractLabeledSyntax  {
  import mutable.{Map=> MMap, Set => MSet, HashMap}

  // Ilya: Can the following guy be self-references, not valuee?
  val concrete : AbstractDirectInterpreter

  val numThreshold: concrete.Val

  // Abstract values are sets
  sealed abstract class AbsNum {
    def +(other: AbsNum): AbsNum = (this, other) match {
      case (_, BeyondThreshold) => BeyondThreshold
      case (BeyondThreshold, _) => BeyondThreshold
      case (Num(n1), Num(n2)) => alpha1(concrete.plus(n1, n2))
    }
    def *(other: AbsNum): AbsNum = (this, other) match {
      case (_, BeyondThreshold) => BeyondThreshold
      case (BeyondThreshold, _) => BeyondThreshold
      case (Num(n1), Num(n2)) => alpha1(concrete.times(n1, n2))
    }
    def isZero: MSet[Boolean] = this match {
      case Num(0)          => MSet(true)
      case BeyondThreshold => MSet(true, false)
      case _ => MSet(false)
    }
  }
  case class Num(n: concrete.Val) extends AbsNum
  case object BeyondThreshold extends AbsNum

  // Primitive abstraction concrete -> abstract
  def alpha1(x: concrete.Val): AbsNum = if (Math.abs(x) < numThreshold) Num(x) else BeyondThreshold

  type Val = MSet[AbsNum]
  def alpha(x: concrete.Val): Val = MSet(alpha1(x)) // singleton abstraction

  type Context = List[Label]
  var ctx: Context = List()

  type Addr = (String, Context)

  val store: MMap[Addr, Val] = new HashMap[Addr, Val] // abstract store

  private def tick(x: String) {
    // TODO: implement ticking -- updating the context for the lates assignment of `x`
  }

  private def upd(k: String, v: Val) { // weak update the abstract store
    tick(k)  // update the context
    val addr = (k, ctx)
    store.put(addr, store.getOrElse(addr, MSet.empty) ++ v)
  }

  private def lookup(k: String): Val = {
    val addr = (k, ctx)
    assert(store.contains(addr)); store(addr)
  }

  // Abstract evaluation of expressions
  //TODO: This implementation *sucks*, as it doesn't account for context sensitivity
  // As a result, everything is coupled with everything (for the same variable)
  def lit(c: Int): Val = alpha(concrete.lit(c))
  def plus(a: Val, b: Val): Val = for (x <- a; y <- b) yield x + y
  def times(a: Val, b: Val): Val = for (x <- a; y <- b) yield x * y
  def ref(a: String): Val = lookup(a)

  // Abstract computation of statements
  type Control = Unit
  def assign(a: String, b: Val): Control = upd(a, b) // update the store
  def if_(c: Val, a: => Control, b: => Control): Control = for (x <- c; isZ <- x.isZero) { if (!isZ) a else b }
  def block(as: List[() => Control]): Control = as.map(_.apply())

  def while_(c: => Val, b: => Control): Control = { // computing a fixpoint
    println(s"-- fixpoint iteration: $label") // TR: testing labels
    val snapshot1 = store.toSet // immutable
    for (x <- c; isZ <- x.isZero) if (!isZ) b
    val snapshot2 = store.toSet
    if (!snapshot2.subsetOf(snapshot1)) while_(c, b)
  }

  type Program = concrete.Val => (Val, MMap[Addr, Val])
  def prog(a: String, b: =>Control, c: =>Val): Program = { x: concrete.Val =>
    store.clear(); upd(a, alpha(x)); b
    (c, store)
  }

}

object TestAbstractCollectingInterpreter extends AbstractInterpreter with AbstractCollectingInterpreter with AbstractLabeling with Examples {

    val concrete = TestAbstractDirectInterpreter
    val numThreshold = 256

    // tests
    def main(args: Array[String]): Unit = {
      val concrRes = concrete.run(concrete.fac)(4)
      val (absRes, absMap) = run(fac)(4)
      // safety condition
      assert(absRes.contains(alpha1(concrRes)))
    }

}


object TestAbstractCollectingInterpreter2 extends AbstractInterpreter with AbstractCollectingInterpreter with AbstractLabeling with Examples {

    val concrete = TestAbstractDirectInterpreter
    val numThreshold = 256

    // tests
    def main(args: Array[String]): Unit = {
      val (absRes, absMap) = run(loop)(5)
      // safety condition
      println(absMap)
    }

}


trait AbstractTransformer extends AbstractSyntax {

  val next: AbstractSyntax

  def preV(a:Val): next.Val
  def postV(a:next.Val): Val

  def preC(a:Control): next.Control
  def postC(a:next.Control): Control

  def postP(a:next.Program): Program


  def lit(c: Int): Val = postV(next.lit(c))
  def plus(a: Val, b: Val): Val = postV(next.plus(preV(a),preV(b)))
  def times(a: Val, b: Val): Val = postV(next.times(preV(a),preV(b)))
  def ref(a: String) = postV(next.ref(a))

  def assign(a: String, b: Val): Control = postC(next.assign(a,preV(b)))
  def block(as: List[() => Control]): Control = postC(next.block(as.map(st => (() => preC(st())))))
  def if_(c: Val, a: => Control, b: => Control): Control = postC(next.if_(preV(c),preC(a),preC(b)))
  def while_(c: => Val, b: => Control): Control = postC(next.while_(preV(c),preC(b)))

  def prog(a: String, b: =>Control, c: =>Val) = postP(next.prog(a,preC(b),preV(c)))

}


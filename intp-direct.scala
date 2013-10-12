package intp.direct

import scala.collection.mutable.HashMap

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
  def block(as: List[Control]): Control
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
    case Block(as)   => block(as.map(exec))
    case If(c,a,b)   => if_(eval(c),exec(a),exec(b))
    case While(c,b)  => while_(eval(c),exec(b))
  }
  def run(p: Prog): Program = p match {
    case Prog(a,b,c) => prog(a,exec(b),eval(c))
  }

}

trait AbstractDirectInterpreter extends AbstractSyntax {
  
  val store = new HashMap[String,Int]

  type Val = Int
  def lit(c: Int): Int = c
  def plus(a: Int, b: Int): Int = a + b
  def times(a: Int, b: Int): Int = a * b
  def ref(a: String) = { assert(store.contains(a)); store(a) }

  type Control = Unit
  def assign(a: String, b: Int): Unit = store(a) = b
  def block(as: List[Unit]): Unit = () 
  def if_(c: Int, a: => Unit, b: => Unit): Unit = if (c != 0) a else b
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

trait AbstractIdentityTransformer extends AbstractSyntax {


}

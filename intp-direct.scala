package intp.direct

import scala.collection.mutable.HashMap

object Test {

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

  case class Prog(a: String, b: Stm, c: Exp)             // p ::= x := input; s*; return e


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
    def run(p: Prog, x: Int): Int = p match {
      case Prog(a,b,c) => store.clear; store(a) = x; exec(b); eval(c)
    }

}
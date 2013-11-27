package intp.pcf

import scala.language.higherKinds

trait FOSyntax {
  type Rep[T]

  def nat(c: Int): Rep[Int]
  def plus(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def times(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def ifnz[T](c: Rep[Int], a: =>Rep[T], b: =>Rep[T]): Rep[T]

  def fix[A,B](f: Rep[A=>B] => Rep[A=>B]): Rep[A=>B]

  type Prog[A,B]
  def prog[A,B](f: Rep[A] => Rep[B]): Prog[A,B]
}

trait Syntax extends FOSyntax {
  def lam[A,B](f: Rep[A] => Rep[B]): Rep[A=>B]
  def app[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B]
}

trait Syntax2 extends FOSyntax {
  // Abstract type for L-function (following Jacques' suggestion)
  type :~>[A, B] // = Rep[Rep[A] => Rep[B]]

  def lam2[A,B](f: Rep[A] => Rep[B]): A :~> B
  def app2[A,B](f: A :~> B, x: Rep[A]): Rep[B]
}

trait DirectInterpreter extends Syntax {

  type Rep[T] = T

  def nat(c: Int): Int = c
  def plus(a: Int, b: Int): Int = a + b
  def times(a: Int, b: Int): Int = a * b
  def ifnz[T](c: Int, a: =>T, b: =>T): T = if (c != 0) a else b

  def lam[A,B](f: A => B): A=>B = f
  def app[A,B](f: A=>B, x: A): B = f(x)
  def fix[A,B](f: (A=>B) => (A=>B)): A=>B = {
    def f1(x:A): B = f(f1)(x)
    f1
  }

  type Prog[A,B] = A => B
  def prog[A,B](f: A => B) = f

}

/**
 * Directly following Carette-al:JFP09, Section 5
 */
trait CBNCPSInterpreter[Res] extends Syntax2 {

  type K[T] = T => Res
  type Rep[T] = K[T] => Res

  type :~>[A, B] = Rep[Rep[A] => Rep[B]]
  //type :~>[A, B] = Rep[A => B]

  def nat(x: Int): Rep[Int] = (k: K[Int]) => k(x)

  def plus(e1: Rep[Int], e2: Rep[Int]): Rep[Int] =
    (k: K[Int]) => e1 ((v1: Int) =>
                   e2 ((v2: Int) => k(v1 + v2)))

  def times(e1: Rep[Int], e2: Rep[Int]): Rep[Int] =
    (k: K[Int]) => e1 ((v1: Int) =>
                   e2 ((v2: Int) => k(v1 * v2)))

  def ifnz[T](eb: Rep[Int], et: =>Rep[T], ee: =>Rep[T]): Rep[T] =
    (k: K[T]) => eb ((vb: Int) => if (vb != 0) et(k) else ee(k))

  def fix[A,B] (g: Rep[Rep[A]=>Rep[B]] => Rep[Rep[A]=>Rep[B]]): Rep[Rep[A]=>Rep[B]] = {
    def fx(f: (A :~> B) => (A :~> B))(x: Rep[A]): Rep[B] =
        app2( f (lam2 ((y:Rep[A]) => fx(f)(y))), x)
    lam2 ((y: Rep[A]) => fx(g)(y))
  }

  def app2[A, B](e1: A :~> B, e2: Rep[A]): Rep[B] =
    (k: K[B]) => e1 ((f : (Rep[A] => Rep[B])) => (f(e2))(k))

  def lam2[A,B](f: Rep[A] => Rep[B]): A :~> B =
    (k: K[Rep[A] => Rep[B]]) => k(f)

  type Prog[A, B] = A :~> B
  def prog[A,B](f: Rep[A] => Rep[B]): Prog[A,B] = lam2(f)

  def run[A](x: Rep[A], k: K[A]): Res = x(k)

}

trait ASTCompiler extends Syntax {

  //TODO: Ilya - can we overcome this burden and generate it (semi-)-automatically?

  // Expressions
  sealed abstract class Exp
  case class Id(x: String) extends Exp
  case class Nat(c: Int) extends Exp
  case class Plus(a: Exp, b: Exp) extends Exp
  case class Times(a: Exp, b: Exp) extends Exp
  case class Ifnz(c: Exp, tb: Exp, eb: Exp) extends Exp
  case class Lam(x: Id, b: Exp) extends Exp
  case class App(t1: Exp, t2: Exp) extends Exp
  case class Fix(t: Exp) extends Exp

  sealed abstract class Value
  case class ValNat(c: Int) extends Value
  case class ValFun(x: Id, body: Exp, env: Env) extends Value

  type Env = Map[Id, Closure]
  case class Closure(t: Exp, e: Env)

  type Rep[_] = Exp

  // --------------------------------------------------------
  var nest = 0
  def nesting(f: Exp => Exp)(x: Exp) = {
    nest += 1; try f(x) finally nest -= 1
  }
  // --------------------------------------------------------

  def nat(c: Int): Rep[Int] = Nat(c)
  def plus(x: Exp, y: Exp): Rep[Int] = Plus(x, y)
  def times(x: Exp, y: Exp): Rep[Int] = Times(x, y)
  def ifnz[T](c: Exp, a: => Exp, b: => Exp): Rep[T] = Ifnz(c, a, b)
  def lam[A,B](f: Exp => Exp): Rep[A=>B] = {
    val x = Id(s"y$nest")
    Lam(x, nesting(f)(x))
  }
  def app[A,B](f: Exp, x: Exp): Rep[B] = App(f, x)
  def fix[A,B](f: Exp => Exp): Rep[A => B] = {
    val x = Id(s"y$nest")
    Fix(Lam(x, nesting(f)(x)))
  }

  // In fact, should be `eval` here
  type Prog[A,B] = Exp
  def prog[A,B](f: Rep[A] => Rep[B]): Prog[A, B] = {
      val x = Id(s"y$nest")
      Lam(x, nesting(f)(x))
  }

}

/**
 * Same as AST compiler, but with closure conversion
 */
trait ClosureConvertingCompiler extends ASTCompiler with Labeling {
  // TODO: implement prog

}


/**
 * An environment-passing interpreter, implementing
 * a Reynolds-style natural semantics in the form of
 * a closure-based evaluation function (see Danvy:ICFP08)
 */
trait EnvironmentPassingInterpreter extends ClosureConvertingCompiler {
  // TODO
}


trait DirectCompiler extends Syntax {

  type Rep[T] = String

  def nat(c: Int): String = s"$c"
  def plus(a: String, b: String): String = s"$a + $b"
  def times(a: String, b: String): String = s"$a * $b"
  def ifnz[T](c: String, a: =>String, b: =>String): String = s"if ($c != 0) $a else $b"

  var nest = 0
  def nesting(f: String => String)(x: String) = { // XX should this produce { y => f(y) } directly?
    nest += 1; try f(x) finally nest -= 1
  }

  def lam[A,B](f: String => String): String = s"lam { y$nest => ${ nesting(f)(s"y$nest")} }"
  def app[A,B](f: String, x: String): String = s"$f($x)"
  def fix[A,B](f: String => String): String = s"fix { y$nest => ${ nesting(f)(s"y$nest")} }"

  type Prog[A,B] = String
  def prog[A,B](f: String => String) = s"prog { y$nest => ${ nesting(f)(s"y$nest")} }"

}


trait Examples extends Syntax {

  // example programs

  // fac(n) = n * fac(n-1)
  def fac = prog[Int,Int] { n =>
    app(fix[Int,Int] { f => lam { n => ifnz(n, times(n,app(f,plus(n,nat(-1)))), nat(1)) } }, n)
  }

}

object TestDirectInterpreter extends DirectInterpreter with Examples {

  // tests

  def main(args: Array[String]): Unit = {
    assert(fac(4) == 24)
  }
}

object TestASTCompiler extends ASTCompiler with Examples {

  def main(args: Array[String]): Unit = {
    println(fac)
    assert(fac ==
      Lam(Id("y0"), App(Fix(Lam(Id("y1"), Lam(Id("y2"), Ifnz(Id("y2"), Times(Id("y2"), App(Id("y1"), Plus(Id("y2"), Nat(-1)))), Nat(1))))), Id("y0"))))
  }

}



object TestDirectCompiler extends DirectCompiler with Examples {

  // tests

  def main(args: Array[String]): Unit = {
    println(fac)
    assert(fac ==
"""prog { y0 => fix { y1 => lam { y2 => if (y2 != 0) y2 * y1(y2 + -1) else 1 } }(y0) }""")

    val str = fac

    import intp.util.ScalaCompile._

    val src = 
"""class Foo extends (Int => Int) {
def prog(f: Int => Int): Int => Int = f
def fix(f: (Int=>Int) => (Int=>Int)): Int => Int = { def f1(x:Int): Int = f(f1)(x); f1 }
def lam(f: Int=>Int): Int => Int = f
def apply(x:Int) = generated(x)
val generated = $str
}
""".replace("$str",str)

    println(src)

    val f = compile[Int,Int](src, "Foo", Nil)

    println(f(4))

    assert(f(4) == 24)

    /*
    TODO:
    add manifest to lam, fix, prog
    add lam, fix, prog operators as prelude, or revise output to not use them
    */

  }

}


trait LabeledSyntax extends Syntax {
  type Label
  def label: Label
}

trait Labeling extends LabeledSyntax {

  abstract class Label
  case object Root extends Label
  case class InExp(up: Label) extends Label
  case class InThen(up: Label) extends Label
  case class InElse(up: Label) extends Label
  case class InLoop(up: Label) extends Label
  case class InLam(up: Label) extends Label
  case class InFix(up: Label) extends Label

  var label: Label = Root
  def block[A](l: Label)(b: => A) = {
    val save = label
    label = l
    try b finally label = save
  }

  def exp[A](b: => Rep[A]) = b

  abstract override def nat(c: Int): Rep[Int]                     = super.nat(c) // 'trivial' expression
  abstract override def plus(x: Rep[Int], y: Rep[Int]): Rep[Int]  = exp(super.plus(x,y))
  abstract override def times(x: Rep[Int], y: Rep[Int]): Rep[Int] = exp(super.times(x,y))
  abstract override def app[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B] = exp(super.app(f,x))

  abstract override def ifnz[T](c: Rep[Int], a: =>Rep[T], b: =>Rep[T]): Rep[T]
    = exp(super.ifnz(c, block(InThen(label))(a), block(InElse(label))(b)))

  abstract override def lam[A,B](f: Rep[A] => Rep[B]): Rep[A=>B] = {
    val static = label
    exp(super.lam(x => block(InLam(static))(f(x))))
  }

  abstract override def fix[A,B](f: Rep[A=>B] => Rep[A=>B]): Rep[A=>B] = {
    val static = label
    exp(super.fix(x => block(InFix(static))(f(x))))
  }

  type Prog[A,B]
  abstract override def prog[A,B](f: Rep[A] => Rep[B]): Prog[A,B] = super.prog(x => block(Root)(f(x)))
}


trait Tracing extends Labeling {

  def store: Any = "?"

  override def block[A](l: Label)(b: => A) =
    super.block(l) { println(s"pp: $label".padTo(50,' ') + s"state: $store"); b }

}


object TestTracingInterpreter extends DirectInterpreter with Tracing with Examples {

  // tests

  def main(args: Array[String]): Unit = {
    assert(fac(4) == 24)
  }
}





trait ANFCompiler extends DirectCompiler with Labeling {

  var code: List[String] = Nil
  var vars = 0

  override def exp[A](b: => Rep[A]) = {
    val i = vars
    code :+= s"val x$i = ${b}\n"
    vars += 1
    s"x$i"
  }


  override def block[A](l: Label)(b: => A) =
    super.block(l) {
      val code0 = code
      val vars0 = vars
      try {
        code = Nil
        var r = b
        (s"{\n${code.mkString}$r\n}").asInstanceOf[A] // XX
      } finally {
        code = code0
        vars = vars0
      }
    }
}

object TestANFCompiler extends ANFCompiler with Examples {
  // tests

  def main(args: Array[String]): Unit = {
    println(fac)
    assert(fac ==
"""prog { y0 => {
val x0 = fix { y1 => {
val x0 = lam { y2 => {
val x0 = if (y2 != 0) {
val x0 = y2 + -1
val x1 = y1(x0)
val x2 = y2 * x1
x2
} else {
1
}
x0
} }
x0
} }
val x1 = x0(y0)
x1
} }""")
  }

}




trait LambdaLiftLCompiler extends DirectCompiler with Labeling {

  var funs: List[String] = Nil

  override def lam[A,B](f: String => String): String = {
    val i = funs.length
    val args = (0 until nest) map (i => s"y$i") mkString(",")
    val static = label
    val f1 = nesting(x => block(InLam(static))(f(x))) _
    funs :+= s"def f$i($args)(y$nest) = ${ f1(s"y$nest")}\n"
    //funs :+= s"def f$i($args) = ${super.lam(f)}"
    exp(s"f$i($args)")
  }

  // TODO: fix

  override def prog[A,B](f: String => String): String = {
    funs = Nil
    val r = super.prog(f)
    s"${funs.mkString}$r"
  }
}

object TestLambdaLiftLCompiler extends LambdaLiftLCompiler with Labeling with Examples {
  // tests

  def main(args: Array[String]): Unit = {
    println(fac)
    assert(fac ==
"""def f0(y0,y1)(y2) = if (y2 != 0) y2 * y1(y2 + -1) else 1
prog { y0 => fix { y1 => f0(y0,y1) }(y0) }""")
  }

}

object TestLLANFCompiler extends ANFCompiler with LambdaLiftLCompiler with Labeling with Examples {
  // tests

  def main(args: Array[String]): Unit = {
    println(fac)
    assert(fac ==
"""def f0(y0,y1)(y2) = {
val x0 = if (y2 != 0) {
val x0 = y2 + -1
val x1 = y1(x0)
val x2 = y2 * x1
x2
} else {
1
}
x0
}
prog { y0 => {
val x0 = fix { y1 => {
val x0 = f0(y0,y1)
x0
} }
val x1 = x0(y0)
x1
} }""")
  }

}

/*

TODO:
+ ScalaCompile: actually run stuff -> debugging
- add TypeRep to emit type annotations
- defunctionalization: replace currying in
  lambda-lift by data types and dispatch method
- CPS conversion: shift/reset
    - if/else
    - function calls
- defunctionalize continuations: remove need for stack,
  obtain plain state transition system
- analysis via abstract domain
    - either interpret low-level program, or
    - generate code to perform analysis
*/


package intp.pcf

trait Syntax {
  type Rep[T]

  def nat(c: Int): Rep[Int]
  def plus(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def times(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def ifnz[T](c: Rep[Int], a: =>Rep[T], b: =>Rep[T]): Rep[T]

  def lam[A,B](f: Rep[A] => Rep[B]): Rep[A=>B]
  def app[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B]
  def fix[A,B](f: Rep[A=>B] => Rep[A=>B]): Rep[A=>B]

  type Prog[A,B]
  def prog[A,B](f: Rep[A] => Rep[B]): Prog[A,B]
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

trait DirectCompiler extends Syntax {

  type Rep[T] = String

  def nat(c: Int): String = s"$c"
  def plus(a: String, b: String): String = s"$a + $b"
  def times(a: String, b: String): String = s"$a * $b"
  def ifnz[T](c: String, a: =>String, b: =>String): String = s"if ($c != 0) $a else $b"

  // note that bindings are not hygienic here! (need labels to assign unique vars)

  def lam[A,B](f: String => String): String = s"lam { x => ${f("x")} }"
  def app[A,B](f: String, x: String): String = s"$f($x)"
  def fix[A,B](f: String => String): String = s"fix { f => ${f("f")} }"

  type Prog[A,B] = String
  def prog[A,B](f: String => String) = f("x")

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


object TestDirectCompiler extends DirectCompiler with Examples {

  // tests

  def main(args: Array[String]): Unit = {
    println(fac)
    assert(fac ==
"""fix { f => lam { x => if (x != 0) x * f(x + -1) else 1 } }(x)""")
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
  def inLabel[A](f: Label => Label)(b: => A) = {
    val save = label
    label = f(label)
    try b finally label = save
  }

  def exp[A](b: => Rep[A]) = inLabel(InExp)(b)

  //abstract override def nat(c: Int): Rep[Int]                     = exp(super.nat(c))
  abstract override def plus(x: Rep[Int], y: Rep[Int]): Rep[Int]  = exp(super.plus(x,y))
  abstract override def times(x: Rep[Int], y: Rep[Int]): Rep[Int] = exp(super.times(x,y))
  abstract override def app[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B] = exp(super.app(f,x))

  abstract override def ifnz[T](c: Rep[Int], a: =>Rep[T], b: =>Rep[T]): Rep[T] 
    = exp(super.ifnz(c, inLabel(InThen)(a), inLabel(InElse)(b)))

  abstract override def lam[A,B](f: Rep[A] => Rep[B]): Rep[A=>B] = {
    val static = label
    exp(super.lam(x => inLabel(_ => InLam(static))(f(x))))
  }

  abstract override def fix[A,B](f: Rep[A=>B] => Rep[A=>B]): Rep[A=>B] = {
    val static = label
    exp(super.fix(x => inLabel(_ => InFix(static))(f(x))))
  }

  type Prog[A,B]
  abstract override def prog[A,B](f: Rep[A] => Rep[B]): Prog[A,B] = super.prog(x => inLabel(_ => Root)(f(x)))
}


trait Tracing extends Labeling {

  def store: Any = "?"

  override def inLabel[A](f: Label => Label)(b: => A) = 
    super.inLabel(f) { println(s"pp: $label".padTo(50,' ') + s"state: $store"); b }

}


object TestTracingInterpreter extends DirectInterpreter with Tracing with Examples {

  // tests

  def main(args: Array[String]): Unit = {
    assert(fac(4) == 24)
  }
}



object TestANFCompiler extends DirectCompiler with Labeling with Examples {

  var code: List[String] = Nil

  override def exp[A](b: => Rep[A]) = {
    val i = code.length
    code :+= s"val x$i = ${b}\n"
    s"x$i"
  }


  override def inLabel[A](f: Label => Label)(b: => A) = 
    super.inLabel(f) { 
      val save = code
      try {
        code = Nil
        var r = b
        (s"{\n${code.mkString}$r\n}").asInstanceOf[A] // XX
      } finally {
        code = save
      }
    }

  // tests

  def main(args: Array[String]): Unit = {
    println(fac)
    assert(fac ==
"""{
val x0 = fix { f => {
val x0 = lam { x => {
val x0 = if (x != 0) {
val x0 = x + -1
val x1 = f(x0)
val x2 = x * x1
x2
} else {
1
}
x0
} }
x0
} }
val x1 = x0(x)
x1
}""")
  }

}


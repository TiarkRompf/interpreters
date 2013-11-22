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

/**
 * Directly following Carette-al:JFP09, Section 5
 */
trait CBNCPSInterpreter[Res] extends Syntax {

  type K[T] = T => Res
  type Rep[T] = K[T] => Res

  // the final continuation
  def runCont[T] : Rep[T]

  // Standard functor map
  def fmap[A, B](f: A=>B, x: Rep[A]): Rep[B] =
    (k: K[B]) => runCont (k.compose(f))

  def nat(x: Int): Rep[Int] = (k: K[Int]) => k(x)

  def plus(e1: Rep[Int], e2: Rep[Int]): Rep[Int] =
    (k: K[Int]) => e1 ((v1: Int) =>
                   e2 ((v2: Int) => k(v1 + v2)))

  def times(e1: Rep[Int], e2: Rep[Int]): Rep[Int] =
    (k: K[Int]) => e1 ((v1: Int) =>
                   e2 ((v2: Int) => k(v1 * v2)))

  def ifnz[T](eb: Rep[Int], et: =>Rep[T], ee: =>Rep[T]): Rep[T] =
    (k: K[T]) => eb ((vb: Int) => if (vb != 0) et(k) else ee(k))

  def fix[A,B] (g: Rep[A=>B] => Rep[A=>B]): Rep[A=>B] = {
    def fx(f: Rep[A=>B] => Rep[A=>B])(x: Rep[A]): Rep[B] = app(f(lam(fx(f))), x)
    lam(fx(g))
  }

  def app[A, B](e1: Rep[A => B], e2: Rep[A]): Rep[B] =
    (k: K[B]) => e1 ((f : (A => B)) => e2(k.compose(f)))

  def lam[A,B](f: Rep[A] => Rep[B]): Rep[A => B] =
    (k: K[A=>B]) => ???

  type Prog[A, B] = A => B
  def prog[A,B](f: Rep[A] => Rep[B]): Prog[A,B] = ???

  def run[A](x: Rep[A], k: K[A]): Res = x(k)

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


object TestDirectCompiler extends DirectCompiler with Examples {

  // tests

  def main(args: Array[String]): Unit = {
    println(fac)
    assert(fac ==
"""fix { f0 => lam { y1 => if (y1 != 0) y1 * f0(y1 + -1) else 1 } }(x)""")
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


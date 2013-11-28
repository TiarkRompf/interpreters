package intp.pcf

import scala.language.higherKinds

trait TypeSyntax {
  case class Typ[T](s: String) { override def toString = s } // TODO: should be abstract type

  implicit def intType: Typ[Int] = Typ("Int")
  implicit def funType[A,B](implicit a:Typ[A], b:Typ[B]): Typ[A=>B] = Typ(s"$a => $b")

  // Note on Scala syntax:
  //   The A:Typ notation used below is syntactic sugar for an implicit parameter 
  //   (think type class instance) of type Typ[A]. So 
  //      lam[A:Typ,B:Typ](f: ...)
  //   is equivalent to
  //      lam[A,B](f: ...)(implicit a: Typ[A], b: Typ[B])
}

trait FOSyntax extends TypeSyntax {
  type Rep[T]

  def nat(c: Int): Rep[Int]
  def plus(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def times(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def ifnz[T:Typ](c: Rep[Int], a: =>Rep[T], b: =>Rep[T]): Rep[T]

  def fix[A:Typ,B:Typ](f: Rep[A=>B] => Rep[A=>B]): Rep[A=>B]

  type Prog[A,B]
  def prog[A:Typ,B:Typ](f: Rep[A] => Rep[B]): Prog[A,B]
}

trait Syntax extends FOSyntax {
  def lam[A:Typ,B:Typ](f: Rep[A] => Rep[B]): Rep[A=>B]
  def app[A:Typ,B:Typ](f: Rep[A=>B], x: Rep[A]): Rep[B]
}

trait Syntax2 extends FOSyntax {
  // Abstract type for L-function (following Jacques' suggestion)
  type :~>[A, B] // = Rep[Rep[A] => Rep[B]]

  def lam2[A:Typ,B:Typ](f: Rep[A] => Rep[B]): A :~> B
  def app2[A:Typ,B:Typ](f: A :~> B, x: Rep[A]): Rep[B]
}




  
trait DirectInterpreter extends Syntax {

  type Rep[T] = T

  def nat(c: Int): Int = c
  def plus(a: Int, b: Int): Int = a + b
  def times(a: Int, b: Int): Int = a * b
  def ifnz[T:Typ](c: Int, a: =>T, b: =>T): T = if (c != 0) a else b

  def lam[A:Typ,B:Typ](f: A => B): A=>B = f
  def app[A:Typ,B:Typ](f: A=>B, x: A): B = f(x)
  def fix[A:Typ,B:Typ](f: (A=>B) => (A=>B)): A=>B = {
    def f1(x:A): B = f(f1)(x)
    f1
  }

  type Prog[A,B] = A => B
  def prog[A:Typ,B:Typ](f: A => B) = f

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
  sealed abstract class Exp[T: Typ]
  case class Id[T: Typ](x: String) extends Exp[T]
  case class Nat(c: Int) extends Exp[Int]
  case class Plus(a: Exp[Int], b: Exp[Int]) extends Exp[Int]
  case class Times(a: Exp[Int], b: Exp[Int]) extends Exp[Int]
  case class Ifnz[T:Typ](c: Exp[Int], tb: Exp[T], eb: Exp[T]) extends Exp[T]
  case class Lam[A:Typ,B:Typ](x: Id[A], b: Exp[B]) extends Exp[A => B]
  case class App[A:Typ,B:Typ](t1: Exp[A=>B], t2: Exp[A]) extends Exp[B]
  case class Fix[A:Typ,B:Typ](t: Exp[(A => B) => (A => B)]) extends Exp[A => B]

  sealed abstract class Value
  case class ValNat(c: Int) extends Value
  case class ValFun[A:Typ,B:Typ](x: Id[A], body: Exp[B], env: Env) extends Value

  type Binding[T] = (Id[T], Closure[T])
  type Env = Set[Binding[_]]
  case class Closure[T](t: Exp[T], e: Env)

  type Rep[T] = Exp[T]

  // --------------------------------------------------------
  var nest = 0
  def nesting[A, B](f: Exp[A] => Exp[B])(x: Exp[A]): Exp[B] = {
    nest += 1; try f(x) finally nest -= 1
  }
  // --------------------------------------------------------

  def nat(c: Int) = Nat(c)
  def plus(x: Exp[Int], y: Exp[Int]) = Plus(x, y)
  def times(x: Exp[Int], y: Exp[Int]) = Times(x, y)
  def ifnz[T:Typ](c: Exp[Int], a: => Exp[T], b: => Exp[T]) = Ifnz(c, a, b)
  def lam[A:Typ, B:Typ](f: Exp[A] => Exp[B]): Exp[A=>B] = {
    val x = Id[A](s"y$nest")
    Lam(x, nesting(f)(x))
  }
  def app[A:Typ, B:Typ](f: Exp[A=>B], x: Exp[A]): Exp[B] = App(f, x)
  def fix[A:Typ, B:Typ](f: Exp[A => B] => Exp[A => B]): Exp[A=>B] = {
    val x = Id[A => B](s"y$nest")
    Fix(Lam(x, nesting(f)(x)))
  }

  // In fact, should be `eval` here
  type Prog[A,B] = Exp[A => B]
  def prog[A:Typ, B:Typ](f: Exp[A] => Exp[B]) = {
      val x = Id[A](s"y$nest")
      Lam(x, nesting(f)(x))
  }

}

/**
 * An environment-passing interpreter, implementing
 * a Reynolds-style natural semantics in the form of
 * a closure-based evaluation function (see Danvy:ICFP08)
 */
trait EnvironmentPassingInterpreter extends ASTCompiler {
  // TODO
}


trait DirectCompiler extends Syntax {

  type Rep[T] = String

  def nat(c: Int): String = s"$c"
  def plus(a: String, b: String): String = s"$a + $b"
  def times(a: String, b: String): String = s"$a * $b"
  def ifnz[T:Typ](c: String, a: =>String, b: =>String): String = s"if ($c != 0) $a else $b"

  var nest = 0
  def nesting(f: String => String)(x: String) = { // XX should this produce { y => f(y) } directly?
    nest += 1; try f(x) finally nest -= 1
  }
  def typ[A:Typ]: String = implicitly[Typ[A]].s

  def lam[A:Typ,B:Typ](f: String => String): String = s"lam { y$nest: ${typ[A]} => ${ nesting(f)(s"y$nest")} }"
  def app[A:Typ,B:Typ](f: String, x: String): String = s"$f($x)"
  def fix[A:Typ,B:Typ](f: String => String): String = s"fix { y$nest: (${typ[A=>B]}) => ${ nesting(f)(s"y$nest")} }"

  type Prog[A,B] = String
  def prog[A:Typ,B:Typ](f: String => String) = s"prog { y$nest: ${typ[A]} => ${ nesting(f)(s"y$nest")} }"

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
    assert(fac.toString ==
      "Lam(Id(y0),App(Fix(Lam(Id(y1),Lam(Id(y2),Ifnz(Id(y2),Times(Id(y2),App(Id(y1),Plus(Id(y2),Nat(-1)))),Nat(1))))),Id(y0)))")
  }

}


trait ScalaLoader extends DirectCompiler {

  def clazz(name: String, parent: String)(body: => String) = 
s"""class $name extends $parent {
$body
}
"""

  def prelude(body: => String) = 
s"""def prog[A,B](f: A => B): A => B = f
def fix[A,B](f: (A=>B) => (A=>B)): A => B = { def f1(x:A): B = f(f1)(x); f1 }
def lam[A,B](f: A=>B): A=>B = f
$body"""

  def main[A:Typ,B:Typ](body: => String) = 
s"""def apply(x: ${typ[A]}) = generated(x)
val generated = {
$body 
}"""

  def load[A:Typ,B:Typ](code: String): A=>B = { // might just override prog
    import intp.util.ScalaCompile
    val name = s"Gen${ ScalaCompile.compileCount }"
    val src = clazz(name, s"(${typ[A]} => ${typ[B]})")(prelude(main[A,B](code)))

    ScalaCompile.dumpGeneratedCode = true
    ScalaCompile.compile[A,B](src, name, Nil)
  }

}


object TestDirectCompiler extends DirectCompiler with ScalaLoader with Examples {

  // tests

  def main(args: Array[String]): Unit = {
    println(fac)
    assert(fac ==
"""prog { y0: Int => fix { y1: (Int=>Int) => lam { y2: Int => if (y2 != 0) y2 * y1(y2 + -1) else 1 } }(y0) }""")

    val f = load[Int,Int](fac)
    assert(f(4) == 24)
  }

}


trait CPSCompiler extends Syntax {

  type Rep[T] = String

  def typ[A:Typ]: String = implicitly[Typ[A]].s

  var nest = 0

  var buf = ""
  def emit(x: String) = buf = buf + ("  "*nest) + x + "\n"

  def exp(x: String, y: String) = { emit(s"val $x = $y"); x }

  def block(sig: String)(body: => String) = {
    val save = nest
    emit(s"def $sig = {")
    nest += 1
    emit(body)
    while (nest > save) {
      nest -= 1
      emit("}")
    }
  }
  def contn(sig: String) = {
    emit(s"def $sig = {")
    nest += 1
  }

  // -----------

  def nat(c: Int) = s"$c"
  def plus(a: Rep[Int], b: Rep[Int]) = exp("xp", s"$a + $b")
  def times(a: Rep[Int], b: Rep[Int]) = exp("xt", s"$a * $b")
  def ifnz[T:Typ](c: Rep[Int], a: =>Rep[T], b: =>Rep[T]) = {
    emit(s"if ($c != 0) fthen() else felse()")
    block(s"fthen()") {
      s"fifnext($a)"
    }
    block(s"felse()") {
      s"fifnext($b)"
    }
    contn(s"fifnext(yif: V)")
    "yif"
  }

  def lam[A:Typ,B:Typ](f: String => String): String = {
    block(s"flam(ylam: V,klam: K)") {
      s"klam(${ f("ylam") })"
    }
    s"clos(flam)"
  }

  def app[A:Typ,B:Typ](f: String, x: String): String = {
    emit(s"$f($x,fappnext)")
    //emit(s"}")
    contn(s"fappnext(yapp: V)")
    "yapp"
  }

  def fix[A:Typ,B:Typ](f: String => String): String = {
    block(s"ffix(yfix:F,kfix:KF)") {
      s"kfix(${ f("yfix") })"
    }
    s"fclos(ffix)"
  }

  type Prog[A,B] = String
  def prog[A:Typ,B:Typ](f: String => String) = {
    block(s"main(ymain:V,kmain:K)") {
      s"kmain(${ f("ymain") })"
    }
    "main(x,y => return y)"
  }

}


trait ScalaLoaderCPS extends CPSCompiler {

  def clazz(name: String, parent: String)(body: => String) = 
s"""class $name extends $parent {
$body
}
"""

  def prelude(body: => String) = 
s"""
type V = Int
type R = Unit
type K = V => R
type F = (V,K) => R
type KF = F => R

def clos(f:F) = f
def fclos(f:(F,KF)=>R): F = {
  def f1(x:V,k:K):R = f(f1, f2 => f2(x,k))
  f1
}
$body"""

  def main[A:Typ,B:Typ](body: => String) = 
s"""def apply(x: ${typ[A]}): ${typ[B]} = {
$body 
}"""

  def load[A:Typ,B:Typ](code: String) = {
    import intp.util.ScalaCompile
    val name = s"Gen${ ScalaCompile.compileCount }"
    val src = clazz(name, s"(${typ[A]} => ${typ[B]})")(prelude(main[A,B](code)))

    ScalaCompile.dumpGeneratedCode = true
    ScalaCompile.compile[A,B](src, name, Nil)
  }

}

object TestCPSCompiler extends CPSCompiler with ScalaLoaderCPS with Examples {

  // tests

  def main(args: Array[String]): Unit = {

    val res = fac
    val code = buf + res + "; -1"

    assert(code ==
"""def main(ymain:V,kmain:K) = {
  def ffix(yfix:F,kfix:KF) = {
    def flam(ylam: V,klam: K) = {
      if (ylam != 0) fthen() else felse()
      def fthen() = {
        val xp = ylam + -1
        yfix(xp,fappnext)
        def fappnext(yapp: V) = {
          val xt = ylam * yapp
          fifnext(xt)
        }
      }
      def felse() = {
        fifnext(1)
      }
      def fifnext(yif: V) = {
        klam(yif)
      }
    }
    kfix(clos(flam))
  }
  fclos(ffix)(ymain,fappnext)
  def fappnext(yapp: V) = {
    kmain(yapp)
  }
}
main(x,y => return y); -1""")


    val f = load[Int,Int](code)
    assert(f(4) == 24)
  }

}







trait LabeledSyntax extends Syntax {
  type Label
  def label: Label
}

trait Labeling extends LabeledSyntax {

  abstract class Label
  case class Root[A,B]()(implicit val arg: Typ[A], res: Typ[B]) extends Label
  case class InThen(up: Label) extends Label
  case class InElse(up: Label) extends Label
  case class InLam[A,B](up: Label)(implicit val arg: Typ[A], val res: Typ[B]) extends Label
  case class InFix[A,B](up: Label)(implicit val arg: Typ[A], val res: Typ[B]) extends Label

  var label: Label = null
  def block[A](l: Label)(b: => A) = {
    val save = label
    label = l
    try b finally label = save
  }

  def exp[A](b: => Rep[A]) = b

  abstract override def nat(c: Int): Rep[Int]                     = super.nat(c) // 'trivial' expression
  abstract override def plus(x: Rep[Int], y: Rep[Int]): Rep[Int]  = exp(super.plus(x,y))
  abstract override def times(x: Rep[Int], y: Rep[Int]): Rep[Int] = exp(super.times(x,y))
  abstract override def app[A:Typ,B:Typ](f: Rep[A=>B], x: Rep[A]): Rep[B] = exp(super.app(f,x))

  abstract override def ifnz[T:Typ](c: Rep[Int], a: =>Rep[T], b: =>Rep[T]): Rep[T]
    = exp(super.ifnz(c, block(InThen(label))(a), block(InElse(label))(b)))

  abstract override def lam[A:Typ,B:Typ](f: Rep[A] => Rep[B]): Rep[A=>B] = {
    val static = label
    exp(super.lam(x => block(InLam[A,B](static))(f(x))))
  }

  abstract override def fix[A:Typ,B:Typ](f: Rep[A=>B] => Rep[A=>B]): Rep[A=>B] = {
    val static = label
    exp(super.fix(x => block(InFix[A,B](static))(f(x))))
  }

  type Prog[A,B]
  abstract override def prog[A:Typ,B:Typ](f: Rep[A] => Rep[B]): Prog[A,B] = super.prog(x => block(Root[A,B])(f(x)))
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

object TestANFCompiler extends ANFCompiler with ScalaLoader with Examples {
  // tests

  def main(args: Array[String]): Unit = {
    println(fac)
    assert(fac ==
"""prog { y0: Int => {
val x0 = fix { y1: (Int => Int) => {
val x0 = lam { y2: Int => {
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

    val f = load[Int,Int](fac)
    assert(f(4) == 24)
  }

}




trait LambdaLiftLCompiler extends DirectCompiler with Labeling {

  var funs: List[String] = Nil

  def typeEnv(c: Label): List[Typ[_]] = c match {
    case c @ Root()     => c.arg :: Nil
    case c @ InLam(up)  => c.arg :: typeEnv(up)
    case c @ InLam(up)  => c.arg :: typeEnv(up)
    case c @ InFix(up)  => funType(c.arg,c.res) /*c.arg*/ :: typeEnv(up)
    case c @ InThen(up) => typeEnv(up)
    case c @ InElse(up) => typeEnv(up)
  }

  override def lam[A:Typ,B:Typ](f: String => String): String = {
    val i = funs.length
    funs :+= "<empty>"
    val static = label
    val types = typeEnv(static).reverse map (_.s)
    assert(types.length == nest)
    val syms = (0 until nest) map (i => s"y$i")
    val parm = (syms,types).zipped map (_ + ":" + _)  mkString ","
    val args = syms mkString ","
    val f1 = nesting(x => block(InLam[A,B](static))(f(x))) _

    val st = s"def f$i($parm)(y$nest:${typ[A]}) = ${ f1(s"y$nest")}\n"
    funs = funs.updated(i,st)
    //funs :+= s"def f$i($args) = ${super.lam(f)}"
    exp(s"f$i($args)_")
  }

  override def fix[A:Typ,B:Typ](f: String => String): String = { // TODO: eliminate code duplication
    val i = funs.length
    funs :+= "<empty>"
    val static = label
    val types = typeEnv(static).reverse map (_.s)
    assert(types.length == nest)
    val syms = (0 until nest) map (i => s"y$i")
    val parm = (syms,types).zipped map (_ + ":" + _)  mkString ","
    val args = syms mkString ","
    val f1 = nesting(x => block(InFix[A,B](static))(f(x))) _
    val st = s"def f$i($parm)(y$nest:${typ[A=>B]}): ${typ[A=>B]} = ${ f1(s"y$nest")}\n"
    funs = funs.updated(i,st)
    //funs :+= s"def f$i($args) = ${super.lam(f)}"
    exp(s"fix(f$i($args))")
  }

  override def prog[A:Typ,B:Typ](f: String => String): String = {
    funs = Nil
    val r = super.prog[A,B](f)
    s"${funs.mkString}$r"
  }
}

object TestLambdaLiftLCompiler extends LambdaLiftLCompiler with Labeling with ScalaLoader with Examples {
  // tests

  def main(args: Array[String]): Unit = {
    println(fac)
    assert(fac ==
"""def f0(y0:Int)(y1:Int => Int): Int => Int = f1(y0,y1)_
def f1(y0:Int,y1:Int => Int)(y2:Int) = if (y2 != 0) y2 * y1(y2 + -1) else 1
prog { y0: Int => fix(f0(y0))(y0) }""")

    val f = load[Int,Int](fac)
    assert(f(4) == 24)
  }

}

object TestLLANFCompiler extends ANFCompiler with LambdaLiftLCompiler with Labeling with ScalaLoader with Examples {
  // tests

  def main(args: Array[String]): Unit = {
    println(fac)
    assert(fac ==
"""def f0(y0:Int)(y1:Int => Int): Int => Int = {
val x0 = f1(y0,y1)_
x0
}
def f1(y0:Int,y1:Int => Int)(y2:Int) = {
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
prog { y0: Int => {
val x0 = fix(f0(y0))
val x1 = x0(y0)
x1
} }""")

    val f = load[Int,Int](fac)
    assert(f(4) == 24)
  }

}

/*

TODO:
+ ScalaCompile: actually run stuff -> debugging
+ add TypeRep to emit type annotations
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

//test
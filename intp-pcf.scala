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
"""prog { y0: Int => fix { y1: (Int => Int) => lam { y2: Int => if (y2 != 0) y2 * y1(y2 + -1) else 1 } }(y0) }""")

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

  def envstr(e: List[(String,String)]) = 
    (e map (p => p._1 + ":" + p._2) mkString ",", e map (p => p._1) mkString ",", e map (p => p._2) mkString ",")

  def exp(x: String, y: String) = { emit(s"val $x = $y"); x }

  def block(name: String, args: List[(String,String)])(body: => String) = {
    val save = nest
    val (mparm,margs,mtps) = envstr(args)
    emit(s"def $name$nest($mparm) = {")
    nest += 1
    emit(body)
    while (nest > save) {
      nest -= 1
      emit("}")
    }
    s"$name$nest"
  }
  def contn(name: String)(body: String => String) = {
    emit(body(s"f$name$nest"))
    emit(s"def f$name$nest(y$name: V) = {")
    nest += 1
    s"y$name"
  }

  def exit: String = "exit" // exit continuation

  // -----------

  def nat(c: Int) = s"$c"
  def plus(a: Rep[Int], b: Rep[Int]) = exp("xp", s"$a + $b")
  def times(a: Rep[Int], b: Rep[Int]) = exp("xt", s"$a * $b")
  def ifnz[T:Typ](c: Rep[Int], a: =>Rep[T], b: =>Rep[T]) = {
    contn("if") { k =>      
      val f1 = block(s"fthen",Nil) {
        s"$k($a)"
      }
      val f2 = block(s"felse",Nil) {
        s"$k($b)"
      }
      s"if ($c != 0) $f1() else $f2()"
    }
  }

  def app[A:Typ,B:Typ](f: String, x: String): String = {
    contn("app") { k =>
      s"$f($x,$k)"
    }
  }

  def lam[A:Typ,B:Typ](f: String => String): String = {
    val f1 = block(s"flam", List(("ylam", "V"),("klam", "K"))) {
      s"klam(${ f("ylam") })"
    }
    s"clos($f1)"
  }

  def fix[A:Typ,B:Typ](f: String => String): String = {
    val f1 = block(s"ffix", List(("yfix","F"),("kfix","KF"))) {
      s"kfix(${ f("yfix") })"
    }
    s"fclos($f1)"
  }

  type Prog[A,B] = String
  def prog[A:Typ,B:Typ](f: String => String) = {
    val f1 = block(s"main", List(("ymain","V"),("kmain","K"))) {
      s"kmain(${ f("ymain") })"
    }
    s"$f1(x,$exit)"
  }

}

trait CPSLLCompiler extends CPSCompiler {

  // perform lambda lifting

  var env: List[List[(String,String)]] = Nil

  var bbuf = ""
  override def emit(x: String) = bbuf = bbuf + x + "\n"

  override def block(name: String, args: List[(String,String)])(body: => String) = {
    val bsave = bbuf
    bbuf = ""
    val save = nest
    val (eparm,eargs,etps) = envstr(env.reverse.flatten)
    val (mparm,margs,mtps) = envstr(args)
    emit(s"def $name$nest($eparm)($mparm) = {")
    nest += 1
    env = args::env
    emit(body)
    nest -= 1
    env = env.tail
    emit("}")
    buf = buf + bbuf
    bbuf = bsave
    s"$name$nest($eargs)"
  }
  override def contn(name: String)(body: String => String) = {
    val (eparm,eargs,etps) = envstr(env.reverse.flatten)
    emit(body(s"f$name${nest-1}($eargs)"))
    nest -= 1
    env = env.tail
    emit("}")
    buf = buf + bbuf
    bbuf = ""
    emit(s"def f$name$nest($eparm)(y$name: V) = {")
    nest += 1
    env = List((s"y$name","V"))::env
    s"y$name"
  }

}

trait CPSDefunCompiler extends CPSCompiler {

  // perform defunctionalization

  var env: List[List[(String,String)]] = Nil

  var dbuf = ""
  def emitd(x: String) = dbuf = dbuf + x + "\n"

  var bbuf = ""
  override def emit(x: String) = bbuf = bbuf + x + "\n"

  override def block(name: String, args: List[(String,String)])(body: => String) = {
    val bsave = bbuf
    bbuf = ""
    val save = nest
    val (eparm,eargs,etps) = envstr(env.reverse.flatten)
    val (mparm,margs,mtps) = envstr(args)
    val mtps1 = if (args.length == 0) "Unit" else mtps
    emitd(s"case class $name$nest($eparm) extends Fun[($mtps1)]")
    emit(s"case ($name$nest($eparm), ($mparm)) =>")
    nest += 1
    env = args::env
    emit(body)
    nest -= 1
    env = env.tail
    //emit("}")
    buf = buf + bbuf
    bbuf = bsave
    s"$name$nest($eargs)"
  }
  override def contn(name: String)(body: String => String) = {
    val (eparm,eargs,etps) = envstr(env.reverse.flatten)
    emit(body(s"f$name${nest-1}($eargs)"))
    nest -= 1
    env = env.tail
    //emit("}")
    buf = buf + bbuf
    bbuf = ""
    emitd(s"case class f$name$nest($eparm) extends Fun[V]")
    emit(s"case (f$name$nest($eparm), y$name: V) =>")
    nest += 1
    env = List((s"y$name","V"))::env
    s"y$name"
  }
  override def exit = {
    s"xexit()"
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
  def f1(x:V,k:K): R = f(f1, f2 => f2(x,k))
  f1
}
$body"""

  def main[A:Typ,B:Typ](body: => String) = 
s"""def apply(x: ${typ[A]}): ${typ[B]} = {
val exit = { (y:V) => return y }
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

    println(code)

    assert(code ==
"""def main0(ymain:V,kmain:K) = {
  def ffix1(yfix:F,kfix:KF) = {
    def flam2(ylam:V,klam:K) = {
      def fthen3() = {
        val xp = ylam + -1
        yfix(xp,fapp4)
        def fapp4(yapp: V) = {
          val xt = ylam * yapp
          fif3(xt)
        }
      }
      def felse3() = {
        fif3(1)
      }
      if (ylam != 0) fthen3() else felse3()
      def fif3(yif: V) = {
        klam(yif)
      }
    }
    kfix(clos(flam2))
  }
  fclos(ffix1)(ymain,fapp1)
  def fapp1(yapp: V) = {
    kmain(yapp)
  }
}
main0(x,exit); -1""")


    val f = load[Int,Int](code)
    assert(f(4) == 24)
  }
}

object TestCPSLLCompiler extends CPSLLCompiler with ScalaLoaderCPS with Examples {

  // tests

  def main(args: Array[String]): Unit = {

    val res = fac
    val code = buf + res + "; -1"

    println(code)

    assert(code ==
"""def fthen3(ymain:V,kmain:K,yfix:F,kfix:KF,ylam:V,klam:K)() = {
val xp = ylam + -1
yfix(xp,fapp3(ymain,kmain,yfix,kfix,ylam,klam))
}
def fapp3(ymain:V,kmain:K,yfix:F,kfix:KF,ylam:V,klam:K)(yapp: V) = {
val xt = ylam * yapp
fif2(ymain,kmain,yfix,kfix,ylam,klam)(xt)
}
def felse3(ymain:V,kmain:K,yfix:F,kfix:KF,ylam:V,klam:K)() = {
fif2(ymain,kmain,yfix,kfix,ylam,klam)(1)
}
def flam2(ymain:V,kmain:K,yfix:F,kfix:KF)(ylam:V,klam:K) = {
if (ylam != 0) fthen3(ymain,kmain,yfix,kfix,ylam,klam)() else felse3(ymain,kmain,yfix,kfix,ylam,klam)()
}
def fif2(ymain:V,kmain:K,yfix:F,kfix:KF,ylam:V,klam:K)(yif: V) = {
klam(yif)
}
def ffix1(ymain:V,kmain:K)(yfix:F,kfix:KF) = {
kfix(clos(flam2(ymain,kmain,yfix,kfix)))
}
def main0()(ymain:V,kmain:K) = {
fclos(ffix1(ymain,kmain))(ymain,fapp0(ymain,kmain))
}
def fapp0(ymain:V,kmain:K)(yapp: V) = {
kmain(yapp)
}
main0()(x,exit); -1""")

    val f = load[Int,Int](code)
    assert(f(4) == 24)
  }
}


object TestCPSDefunCompiler extends CPSDefunCompiler with ScalaLoaderCPS with Examples {

  // tests
  override def prelude(body: => String) = // XX: using tuples instead of multi-arg functions
s"""
type V = Int
type R = Unit
type K = V => R
type F = ((V,K)) => R
type KF = F => R

$body"""

  def decls(types: String, dispatch: String) = // XX make part of prog?
    "def clos(f:F) = f\n" +
    "def fclos(f:((F,KF))=>R): F = xfix(f)\n" +
    "abstract class Fun[T] extends (T=>Unit) with Product { \n" +
    "override def toString = productPrefix + '(' + productIterator.mkString(\",\") + ')'\n" +
    "def apply(x:T) = { println(\"call \"+this+\" -- \"+x); call(this,x) }}\n" +
    "case class xexit() extends Fun[V]\n" +
    "case class xfix(f:((F,KF))=>R) extends Fun[(V,K)]\n" +
    "case class xpart(x:V,k:K) extends Fun[F]\n" +
    types + 
    "def call[T](f: Fun[T], x: T): Unit = (f,x) match {\n" +
    "case (xexit(), y: V) =>\n" +
    "exit(y)\n" +
    "case (f1@xfix(f:(((F,KF))=>R)), (x:V,k:K)) =>\n" +
    "f((f1,xpart(x,k)))\n" +
    "case (xpart(x,k), (f:F)) =>\n" +
    "f(x,k)\n" +
    dispatch + 
    "}\n"


  def main(args: Array[String]): Unit = {

    val res = fac
    val code = decls(dbuf,buf) + res + "; -1"

    println(code)

    assert(code ==
"""def clos(f:F) = f
def fclos(f:((F,KF))=>R): F = xfix(f)
abstract class Fun[T] extends (T=>Unit) with Product { 
override def toString = productPrefix + '(' + productIterator.mkString(",") + ')'
def apply(x:T) = { println("call "+this+" -- "+x); call(this,x) }}
case class xexit() extends Fun[V]
case class xfix(f:((F,KF))=>R) extends Fun[(V,K)]
case class xpart(x:V,k:K) extends Fun[F]
case class main0() extends Fun[(V,K)]
case class ffix1(ymain:V,kmain:K) extends Fun[(F,KF)]
case class flam2(ymain:V,kmain:K,yfix:F,kfix:KF) extends Fun[(V,K)]
case class fthen3(ymain:V,kmain:K,yfix:F,kfix:KF,ylam:V,klam:K) extends Fun[(Unit)]
case class fapp3(ymain:V,kmain:K,yfix:F,kfix:KF,ylam:V,klam:K) extends Fun[V]
case class felse3(ymain:V,kmain:K,yfix:F,kfix:KF,ylam:V,klam:K) extends Fun[(Unit)]
case class fif2(ymain:V,kmain:K,yfix:F,kfix:KF,ylam:V,klam:K) extends Fun[V]
case class fapp0(ymain:V,kmain:K) extends Fun[V]
def call[T](f: Fun[T], x: T): Unit = (f,x) match {
case (xexit(), y: V) =>
exit(y)
case (f1@xfix(f:(((F,KF))=>R)), (x:V,k:K)) =>
f((f1,xpart(x,k)))
case (xpart(x,k), (f:F)) =>
f(x,k)
case (fthen3(ymain:V,kmain:K,yfix:F,kfix:KF,ylam:V,klam:K), ()) =>
val xp = ylam + -1
yfix(xp,fapp3(ymain,kmain,yfix,kfix,ylam,klam))
case (fapp3(ymain:V,kmain:K,yfix:F,kfix:KF,ylam:V,klam:K), yapp: V) =>
val xt = ylam * yapp
fif2(ymain,kmain,yfix,kfix,ylam,klam)(xt)
case (felse3(ymain:V,kmain:K,yfix:F,kfix:KF,ylam:V,klam:K), ()) =>
fif2(ymain,kmain,yfix,kfix,ylam,klam)(1)
case (flam2(ymain:V,kmain:K,yfix:F,kfix:KF), (ylam:V,klam:K)) =>
if (ylam != 0) fthen3(ymain,kmain,yfix,kfix,ylam,klam)() else felse3(ymain,kmain,yfix,kfix,ylam,klam)()
case (fif2(ymain:V,kmain:K,yfix:F,kfix:KF,ylam:V,klam:K), yif: V) =>
klam(yif)
case (ffix1(ymain:V,kmain:K), (yfix:F,kfix:KF)) =>
kfix(clos(flam2(ymain,kmain,yfix,kfix)))
case (main0(), (ymain:V,kmain:K)) =>
fclos(ffix1(ymain,kmain))(ymain,fapp0(ymain,kmain))
case (fapp0(ymain:V,kmain:K), yapp: V) =>
kmain(yapp)
}
main0()(x,xexit()); -1""")

/*
call main0() -- (4,xexit())
call xfix(ffix1(4,xexit())) -- (4,fapp0(4,xexit()))
call ffix1(4,xexit()) -- (xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())))
call xpart(4,fapp0(4,xexit())) -- flam2(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())))
call flam2(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit()))) -- (4,fapp0(4,xexit()))
call fthen3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())) -- ()
call xfix(ffix1(4,xexit())) -- (3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))
call ffix1(4,xexit()) -- (xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))
call xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))) -- flam2(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))
call flam2(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))) -- (3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))
call fthen3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))) -- ()
call xfix(ffix1(4,xexit())) -- (2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))
call ffix1(4,xexit()) -- (xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))))
call xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))) -- flam2(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))))
call flam2(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))) -- (2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))
call fthen3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))) -- ()
call xfix(ffix1(4,xexit())) -- (1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))))
call ffix1(4,xexit()) -- (xfix(ffix1(4,xexit())),xpart(1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))))
call xpart(1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))) -- flam2(4,xexit(),xfix(ffix1(4,xexit())),xpart(1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))))
call flam2(4,xexit(),xfix(ffix1(4,xexit())),xpart(1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))))) -- (1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))))
call fthen3(4,xexit(),xfix(ffix1(4,xexit())),xpart(1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))),1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))) -- ()
call xfix(ffix1(4,xexit())) -- (0,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))),1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))))
call ffix1(4,xexit()) -- (xfix(ffix1(4,xexit())),xpart(0,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))),1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))))))
call xpart(0,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))),1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))))) -- flam2(4,xexit(),xfix(ffix1(4,xexit())),xpart(0,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))),1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))))))
call flam2(4,xexit(),xfix(ffix1(4,xexit())),xpart(0,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))),1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))))) -- (0,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))),1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))))
call felse3(4,xexit(),xfix(ffix1(4,xexit())),xpart(0,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))),1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))))),0,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))),1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))))) -- ()
call fif2(4,xexit(),xfix(ffix1(4,xexit())),xpart(0,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))),1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))))),0,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))),1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))))) -- 1
call fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))),1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))) -- 1
call fif2(4,xexit(),xfix(ffix1(4,xexit())),xpart(1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))),1,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))))) -- 1
call fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))) -- 1
call fif2(4,xexit(),xfix(ffix1(4,xexit())),xpart(2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))),2,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())))) -- 2
call fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))) -- 2
call fif2(4,xexit(),xfix(ffix1(4,xexit())),xpart(3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))),3,fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit()))) -- 6
call fapp3(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())) -- 6
call fif2(4,xexit(),xfix(ffix1(4,xexit())),xpart(4,fapp0(4,xexit())),4,fapp0(4,xexit())) -- 24
call fapp0(4,xexit()) -- 24
call xexit() -- 24
*/

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
+ CPS conversion: shift/reset
    - if/else
    - function calls
- defunctionalization: replace currying in
  lambda-lift by data types and dispatch method
- defunctionalize continuations: remove need for stack,
  obtain plain state transition system
- analysis via abstract domain
    - either interpret low-level program, or
    - generate code to perform analysis
- what about syntactic optimizations:
    - cps administrative redexes
    - lambda lift unused vars
*/

//test
package lambda.untyped

import cats.Id
import cats.arrow.FunctionK
import lambda.untyped.Lam.Name


trait Lam[A] {
  def lam(name: Name, term: A): A
  def app(f: A, x: A): A
  def v(name: Name): A
}

object Lam {
  type Name = Int

  type Expr = FunctionK[Lam, Id]

  object make {
    type T
    def apply(f: Lam[T] => T): Expr = new FunctionK[Lam, Id] {
      def apply[A](fa: Lam[A]): Id[A] = f(fa.asInstanceOf[Lam[T]]).asInstanceOf[A]
    }
  }

  def lam[A](name: Name, term: A)(implicit lam: Lam[A]): A = lam.lam(name, term)
  def app[A](f: A, x: A)(implicit lam: Lam[A]): A = lam.app(f, x)
  def app2[A: Lam](f: A, x: A, y: A): A = app(app(f, x), y)
  def v[A](name: Name)(implicit lam: Lam[A]): A = lam.v(name)

  val X = Builder(0)
  val Y = Builder(1)
  val Z = Builder(2)
  val A = Builder(3)
  val B = Builder(4)

  final case class Builder(idx: Name) extends AnyVal {
    def v[A: Lam]: A = Lam.v(idx)
    def lam[A: Lam](term: A): A = Lam.lam(idx, term)
    def app[A: Lam](term: A): A = Lam.app(v, term)
  }

  def id[A: Lam]: A = X.lam(X.v)
  def False[A: Lam]: A = X.lam(Y.lam(X.v))
  def True[A: Lam]: A = X.lam(Y.lam(Y.v))

  def zero[A: Lam]: A = False
  def one[A: Lam]: A = X.lam(Y.lam(X.app(Y.v)))
  def two[A: Lam]: A = X.lam(Y.lam(X.app(X.app(Y.v))))

  def succ[A: Lam](n: A): A = X.lam(Y.lam(X.app(app2(n, X.v, Y.v))))
  def plus[A: Lam](a: A, b: A): A = X.lam(Y.lam(app2(a, X.v, app2(b, X.v, Y.v))))
  def add[A: Lam]: A = A.lam(B.lam(plus(A.v, B.v)))
  def times[A: Lam](a: A, b: A): A = app2(a, app(add, b), zero)
  def mul[A: Lam]: A = A.lam(B.lam(times(A.v, B.v)))
  def exp[A: Lam](a: A, b: A): A = app2(b, app(mul, a), one)
  def pow[A: Lam]: A = A.lam(B.lam(exp(A.v, B.v)))

  val ALPHA = "" ++ ('x' to 'z') ++ ('a' to 'w')

  implicit object print extends Lam[String] {
    def nameIdx(name: Name) = {
      val letter = ALPHA(name % ALPHA.length).toString
      val idx = name / ALPHA.length - 1
      if (idx < 0) letter else s"$letter$idx"
    }
    def lam(name: Name, term: String): String = s"λ${nameIdx(name)}.$term"
    def app(f: String, x: String): String = s"($f $x)"
    def v(name: Name): String = nameIdx(name)
  }
}


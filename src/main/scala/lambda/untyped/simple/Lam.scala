package lambda.untyped.simple

import cats.Id
import cats.arrow.FunctionK
import lambda.untyped.simple.Lam.{ALPHA, Name}

trait Lam[A] {
  def lam(name: Name, term: A): A
  def app(f: A, x: A): A
  def v(name: Name): A
}

object Lam extends LamStd {
  type Name = Int

  type Expr = FunctionK[Lam, Id]

  object make {
    type T
    def apply(f: Lam[T] => T): Expr = new FunctionK[Lam, Id] {
      def apply[A](fa: Lam[A]): Id[A] = f(fa.asInstanceOf[Lam[T]]).asInstanceOf[A]
    }
  }
  val ALPHA = "" ++ ('x' to 'z') ++ ('a' to 'w')
}

class LamStd {
  final implicit object print extends Lam[String] {
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


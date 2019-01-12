package lambda.untyped.debrujin

import cats.Id
import cats.arrow.FunctionK


trait Lam[A] {
  def lam(term: A): A
  def app(f: A, x: A): A
  def v(name: Int): A
}

object Lam extends LamStd {
  type Expr = FunctionK[Lam, Id]

  object make {
    type T
    def apply(f: Lam[T] => T): Expr = new FunctionK[Lam, Id] {
      def apply[A](fa: Lam[A]): Id[A] = f(fa.asInstanceOf[Lam[T]]).asInstanceOf[A]
    }
  }
}

class LamStd {
  final implicit object print extends Lam[String] {
    def lam(term: String): String = s"λ.$term"
    def app(f: String, x: String): String = s"($f $x)"
    def v(name: Int): String = name.toString
  }
}


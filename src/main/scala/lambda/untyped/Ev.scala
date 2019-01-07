package lambda.untyped

import cats.Eval
import lambda.untyped.Lam.Name

sealed trait Ev[A]

object Ev {
  final case class Val[A](a: Eval[A]) extends Ev[A]
  final case class Fun[A](f: Eval[A => Eval[A]]) extends Ev[A]

  implicit def lambda[A]: Lam[Ev[A]] = new Lam[Ev[A]] {
    def lam(name: Name, term: Ev[A]): Ev[A] = ???
    def app(f: Ev[A], x: Ev[A]): Ev[A] = ???
    def v(name: Name): Ev[A] = ???
  }
}

package lambda.untyped

import cats.Eval
import lambda.untyped.Lam.Name
import cats.syntax.apply._
import Eval.now
import terms._

sealed trait Ev[A]

object Ev {
  type Run[A] = Map[Name, Eval[Ev[A]]] => Eval[Ev[A]]

  def run[A](run: Run[A]): Either[String, A] =
    run(Map.empty).value match {
      case Val(a1) => Right(a1.value)
      case Error(str) => Left(str)
      case Fun(_) => Left("result is a function")
    }

  def lift[A](f: A => A): Run[A] = m => now(Fun(now((ex: Eval[Ev[A]]) => ex.flatMap {
    case Val(ea) => ea.map(a => Val(now(f(a))))
    case err@Error(_) => now(err)
    case Fun(_) => now(Error("could apply lifted function to function"))
  })))

  def pure[A](x: A): Run[A] = m => now(Val(now(x)))

  def runInt(term: Run[Int]): Either[String, Int] = run(term ! lift(_ + 1) ! pure(0))
  def runBool(term: Run[Boolean]): Either[String, Boolean] = run(term ! pure(true) ! pure(false))

  final case class Val[A](a: Eval[A]) extends Ev[A]
  final case class Error[A](s: String) extends Ev[A]
  final case class Fun[A](f: Eval[Eval[Ev[A]] => Eval[Ev[A]]]) extends Ev[A]

  implicit def lambda[A]: Lam[Run[A]] = new Lam[Run[A]] {
    def lam(name: Name, term: Run[A]): Run[A] =
      m => now(Fun[A](now(x => term(m + (name -> x)))))
    def app(f: Run[A], x: Run[A]): Run[A] = m => f(m).flatMap {
      case Error(str) => now(Error(str))
      case Val(ea) => now(Error(s"cound not apply ${ea.value} as function"))
      case Fun(ef) => ef.flatMap(ff => ff(x(m)))
    }
    def v(name: Name): Run[A] = m => m.getOrElse(name, now(Error(s"not found $name")))
  }
}

package lib
import cats.Monoid
import cats.syntax.monoid._
import cats.instances.unit._
import cats.instances.long._
import cats.instances.vector._
import Monoid.empty

final case class iter[A](start: A)(step: A => Option[A]) {
  def iterCollect[M: Monoid](measure: A => M): (A, M) = {
    def go(cur: A, m: M): (A, M) =
      step(cur) match {
        case None => (cur, m)
        case Some(a) => go(a, m |+| measure(cur))
      }
    go(start, empty[M])
  }

  def run: A = iterCollect(_ => ())._1

  def count: (A, Long) = iterCollect(_ => 1L)

  def steps: Vector[A] = iterCollect(a => Vector(a)) match {
    case (a, m) => m :+ a
  }
}

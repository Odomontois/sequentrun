package util

trait Equals[A, B] {
  def subst[F[_]](f: F[A]): F[B]
  def inv: Equals[B, A] = subst[Equals[?, A]](Equals.id)
}

object Equals {
  def id[A]: Equals[A, A] = new Equals[A, A] {
    def subst[F[_]](f: F[A]): F[A] = f
  }
}



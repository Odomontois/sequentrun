package lib

trait Equals[A, B] {
  def apply[F[_]](f: F[A]): F[B]
  def inv: Equals[B, A] = apply[Equals[?, A]](Equals.id)
}

object Equals {
  def id[A]: Equals[A, A] = new Equals[A, A] {
    def apply[F[_]](f: F[A]): F[A] = f
  }
}



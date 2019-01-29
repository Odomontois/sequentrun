package lib

trait Mat[A] {
  def materialize: A
}

object Mat {
  def apply[A](implicit mat: Mat[A]): Mat[A] = mat

  def mat[A](implicit mat: Mat[A]): A = mat.materialize
}

trait U[M[_]] {
  def reify[A](mat: M[A]): A
}

object U {


  implicit def reifyMat[M[x] <: Mat[x]]: U[M] = new U[M] {
    def reify[A](mat: M[A]): A = mat.materialize
  }
}


final case class Reified[A](reify: A) extends AnyVal

object Reified{
  def reify[A](implicit r: Reified[A]): A = r.reify

  implicit def uniReified[A, M[_]](implicit mat: M[A], u: U[M]): Reified[A] = Reified(u.reify(mat))
}
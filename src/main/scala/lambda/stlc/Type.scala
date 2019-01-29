package lambda.stlc
import lambda.stlc.Type.{-->, Elim, ElimEq, ⍟}
import lib.{~=, Equals}

trait Type[T] {
  def elimEq[F[_]](elim: ElimEq[F, T]): F[T]

  def elim[F[_]](e: Elim[F]): F[T] =
    elimEq(new ElimEq[F, T] {
      def star(eq: T ~= ⍟): F[⍟]                                = e.star
      def arr[A: Type, B: Type](eq: T ~= (A --> B)): F[A --> B] = e.arr[A, B]
    })

  def materialize: T
}

object Type {
  def apply[T](implicit T: Type[T]): Type[T] = T

  type ⍟ = ⍟.type
  case object ⍟
  final case class -->[A, B](a: A, b: B)

  trait ElimEq[F[_], T] {
    def star(eq: T ~= ⍟): F[⍟]
    def arr[A: Type, B: Type](eq: T ~= (A --> B)): F[A --> B]

    def elim(implicit T: Type[T]): F[T] = T.elimEq(this)
  }

  trait Elim[F[_]] {
    def star: F[⍟]
    def arr[A: Type, B: Type]: F[A --> B]

    def elim[T: Type]: F[T] = Type[T].elim(this)
  }

  implicit val starType: Type[⍟] = new Type[⍟] {
    def elimEq[F[_]](elim: ElimEq[F, ⍟]): F[⍟] = elim.star(Equals.id)
    val materialize: ⍟                         = ⍟
  }

  implicit def arrowType[A: Type, B: Type]: Type[A --> B] = new Type[A --> B] {
    def elimEq[F[_]](elim: ElimEq[F, A --> B]): F[A --> B] = elim.arr[A, B](Equals.id)
    val materialize: A --> B                               = -->(Type[A].materialize, Type[B].materialize)
  }
}

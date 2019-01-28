package lambda.stlc
import cats.kernel.Comparison.EqualTo
import lambda.stlc.Env.{:::, Elim, ElimEq, Empty}
import util.{Equals, ~=}

trait Env[E] {
  def elimEq[F[_]](elim: ElimEq[F, E]): F[E]

  def elim[F[_]](el: Elim[F]): F[E] =
    elimEq(new ElimEq[F, E] {
      def empty(eq: E ~= Empty): F[Empty]                                 = el.empty
      def cons[H: Type, T: Env](eq: E ~= Env.:::[H, T]): F[Env.:::[H, T]] = el.cons[H, T]
    })

  def materialize: E
}

object Env {
  def apply[T](implicit env: Env[T]): Env[T] = env

  type Empty = Empty.type
  case object Empty
  final case class :::[H: Type, T: Env](head: H, tail: T)

  trait ElimEq[F[_], E] {
    def empty(eq: E ~= Empty): F[Empty]
    def cons[H: Type, T: Env](eq: E ~= (H ::: T)): F[H ::: T]

    def elim(implicit env: Env[E]): F[E] = env.elimEq(this)
  }

  trait Elim[F[_]] {
    def empty: F[Empty]
    def cons[H: Type, T: Env]: F[H ::: T]

    def elim[T: Env]: F[T] = Env[T].elim(this)
  }

  implicit val emptyEnv: Env[Empty] = new Env[Empty] {
    def elimEq[F[_]](elim: ElimEq[F, Empty]): F[Empty] = elim.empty(Equals.id)
    def materialize: Empty                             = Empty
  }

  implicit def consEnv[H: Type, T: Env]: Env[:::[H, T]] = new Env[H ::: T] {
    def elimEq[F[_]](elim: ElimEq[F, H ::: T]): F[H ::: T] = elim.cons[H, T](Equals.id)
    def materialize: H ::: T                               = :::(Type[H].materialize, Env[T].materialize)
  }
}

sealed abstract class Elem[T: Type, E: Env] {
  def get(e: E): T
}

object Elem {
  final case class Here[T: Type, E: Env]() extends Elem[T, T ::: E] {
    def get(e: T ::: E): T = e.head
  }
  final case class There[T: Type, E: Env, H: Type](elem: Elem[T, E]) extends Elem[T, H ::: E] {
    def get(e: H ::: E): T = elem.get(e.tail)
  }
}

trait SubSet[E, E1] {
  def find[T: Type](elem: Elem[T, E]): Elem[T, E1]
}

object SubSet {
  def emptyIsSubset[L: Env]: SubSet[Empty, L] = ???
}

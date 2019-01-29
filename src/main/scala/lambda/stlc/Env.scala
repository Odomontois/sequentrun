package lambda.stlc
import cats.kernel.Comparison.EqualTo
import lambda.stlc.Elem.{Here, There}
import lambda.stlc.Env.{:::, Elim, ElimEq, Empty}
import lib._

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

  case object Empty                                       extends LeftVariant
  final case class :::[H: Type, T: Env](head: H, tail: T) extends RightVariant

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
  type Ev = E
  def get(e: E): T
  def elim[R](e: Elem.ElimEq[T, E, R]): R
}

object Elem {
  trait ElimEq[T, E, R] {
    def here[Tail: Env](eqs: E ~= (T ::: Tail)): R
    def there[H: Type, Tail: Env](elem: Elem[T, Tail], eqs: E ~= (H ::: Tail)): R
  }

  final case class Here[T: Type, E: Env]() extends Elem[T, T ::: E] {
    def get(e: T ::: E): T                   = e.head
    def elim[R](e: ElimEq[T, T ::: E, R]): R = e.here(Equals.id[T ::: E])
  }
  final case class There[T: Type, E: Env, H: Type](elem: Elem[T, E]) extends Elem[T, H ::: E] {
    def get(e: H ::: E): T                   = elem.get(e.tail)
    def elim[R](e: ElimEq[T, H ::: E, R]): R = e.there[H, E](elem, Equals.id[H ::: E])
  }
}

trait SubSet[E, E1] {
  def find[T: Type](elem: Elem[T, E]): Elem[T, E1]
}

object SubSet {
  def emptyIsSubset[L: Env]: SubSet[Empty, L] = new SubSet[Empty, L] {
    def find[T: Type](elem: Elem[T, Empty]): Elem[T, L] =
      elem.elim {
        new Elem.ElimEq[T, Empty, Elem[T, L]] {
          def here[Tail: Env](eqs: Empty ~= (T ::: Tail)): Elem[T, L] =
            Disjoint.absurd(eqs[λ[a => Empty with a]](Empty))
          def there[H: Type, Tail: Env](elem: Elem[T, Tail], eqs: Empty ~= (H ::: Tail)): Elem[T, L] =
            Disjoint.absurd(eqs[λ[a => Empty with a]](Empty))
        }
      }
  }

  def rightWeak[H: Type, E: Env, E1: Env](subSet: SubSet[E, E1]): SubSet[E, H ::: E1] =
    new SubSet[E, H ::: E1] {
      def find[T: Type](elem: Elem[T, E]): Elem[T, H ::: E1] =
        There(subSet.find[T](elem))
    }

  def leftWeak[H: Type, E: Env, E1: Env](subSet: SubSet[H ::: E, E1]): SubSet[E, E1] =
    new SubSet[E, E1] {
      def find[T: Type](elem: Elem[T, E]): Elem[T, E1] =
        subSet.find(There(elem))
    }
}

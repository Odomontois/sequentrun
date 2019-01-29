package lib
import lib.DList.{:::, Elim, ElimEq, Empty}
import Reified.reify
import lib.Elem.{Here, There}

trait DList[T[_], E] extends Mat[E] {
  def elimEq[F[_]](elim: ElimEq[F, T, E]): F[E]

  def elim[F[_]](el: Elim[F, T]): F[E] =
    elimEq(new ElimEq[F, T, E] {
      def empty(eq: E ~= Empty): F[Empty]                                     = el.empty
      def cons[H: T, Tail: DList[T, ?]](eq: E ~= (H ::: Tail)): F[H ::: Tail] = el.cons[H, Tail]
    })
}

object DList {
  def apply[M[_], L](implicit dlist: DList[M, L]): DList[M, L] = dlist

  type Empty = Empty.type

  case object Empty                            extends LeftVariant
  final case class :::[H, T](head: H, tail: T) extends RightVariant { type Head = H; type Tail = T }

  trait ElimEq[F[_], M[_], E] {
    def empty(eq: E ~= Empty): F[Empty]
    def cons[H: M, T: DList[M, ?]](eq: E ~= (H ::: T)): F[H ::: T]

    def elim(implicit env: DList[M, E]): F[E] = env.elimEq(this)
  }

  trait Elim[F[_], M[_]] {
    def empty: F[Empty]
    def cons[H: M, T: DList[M, ?]]: F[H ::: T]

    def elim[L: DList[M, ?]]: F[L] = DList[M, L].elim(this)
  }

  implicit def emptyEnv[M[_]]: DList[M, Empty] = new DList[M, Empty] {
    def elimEq[F[_]](elim: ElimEq[F, M, Empty]): F[Empty] = elim.empty(Equals.id)
    def materialize: Empty                                = Empty
  }

  implicit def consEnv[M[x]: U, H: M, T: DList[M, ?]]: DList[M, H ::: T] = new DList[M, H ::: T] {
    def elimEq[F[_]](elim: ElimEq[F, M, H ::: T]): F[H ::: T] = elim.cons[H, T](Equals.id)
    val materialize: H ::: T                                  = :::(reify[H], reify[T])
  }
}

sealed abstract class Elem[M[x], X: M, L: DList[M, ?]] {
  type Ev = L
  def get(e: L): X
  def elim[R](e: Elem.ElimEq[M, X, L, R]): R
}

object Elem {
  trait ElimEq[M[_], X, L, R] {
    def here[Tail: DList[M, ?]](eqs: L ~= (X ::: Tail)): R
    def there[H: M, Tail: DList[M, ?]](elem: Elem[M, X, Tail], eqs: L ~= (H ::: Tail)): R
  }

  final case class Here[M[x]: U, X: M, L: DList[M, ?]]() extends Elem[M, X, X ::: L] {
    def get(e: X ::: L): X                      = e.head
    def elim[R](e: ElimEq[M, X, X ::: L, R]): R = e.here(Equals.id[X ::: L])
  }

  final case class There[M[x]: U, X: M, L: DList[M, ?], H: M](elem: Elem[M, X, L]) extends Elem[M, X, H ::: L] {
    def get(e: H ::: L): X                      = elem.get(e.tail)
    def elim[R](e: ElimEq[M, X, H ::: L, R]): R = e.there[H, L](elem, Equals.id[H ::: L])
  }
}

trait SubSet[M[_], L, L1] {
  def find[X: M](elem: Elem[M, X, L]): Elem[M, X, L1]
}

object SubSet {
  def emptyIsSubset[M[_], L: DList[M, ?]]: SubSet[M, Empty, L] = new SubSet[M, Empty, L] {
    def find[X: M](elem: Elem[M, X, Empty]): Elem[M, X, L] =
      elem.elim {
        new Elem.ElimEq[M, X, Empty, Elem[M, X, L]] {
          def here[Tail: DList[M, ?]](eqs: Empty ~= (X ::: Tail)): Elem[M, X, L] =
            Disjoint.absurd(eqs[λ[a => Empty with a]](Empty))
          def there[H: M, Tail: DList[M, ?]](elem: Elem[M, X, Tail], eqs: Empty ~= (H ::: Tail)): Elem[M, X, L] =
            Disjoint.absurd(eqs[λ[a => Empty with a]](Empty))
        }
      }
  }

  def rightWeak[M[_]: U, H: M, L: DList[M, ?], L1: DList[M, ?]](subSet: SubSet[M, L, L1]): SubSet[M, L, H ::: L1] =
    new SubSet[M, L, H ::: L1] {
      def find[T: M](elem: Elem[M, T, L]): Elem[M, T, H ::: L1] = There(subSet.find[T](elem))
    }

  def leftWeak[M[_]: U, H: M, L: DList[M, ?], L1: DList[M, ?]](subSet: SubSet[M, H ::: L, L1]): SubSet[M, L, L1] =
    new SubSet[M, L, L1] {
      def find[X: M](elem: Elem[M, X, L]): Elem[M, X, L1] = subSet.find(There(elem))
    }

  def contract[M[_]: U, A: M, L: DList[M, ?], L1: DList[M, ?]](subSet: SubSet[M, L, L1],
                                                               has: Elem[M, A, L]): SubSet[M, A ::: L, L1] =
    new SubSet[M, A ::: L, L1] {
      def find[X: M](elem: Elem[M, X, A ::: L]): Elem[M, X, L1] =
        elem.elim(new Elem.ElimEq[M, X, A ::: L, Elem[M, X, L1]] {
          def here[Tail: DList[Any, ?]](eqs: (A ::: L) ~= (X ::: Tail)): Elem[M, X, L1] = ???
          def there[H: M, Tail: DList[Any, ?]](elem: Elem[M, X, Tail], eqs: (A ::: L) ~= (H ::: Tail)): Elem[M, X, L1] =
            ???
        })
    }
}

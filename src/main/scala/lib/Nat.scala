package lib
import cats.Show
import cats.kernel.Order
import lib.CompareSubst.{EQ, GT, LT}
import lib.Nat.{Succ, Zero}
import lib.Fin.{FSucc, FZero}

trait Nat[N] {
  def elim[F[_]](f: Nat.Elim[F]): F[N] =
    elimEq(new Nat.ElimEq[F, N] {
      def zero(eq: N ~= Zero): F[Zero]                     = f.zero
      def succ[P: Nat](n: P)(eq: N ~= Succ[P]): F[Succ[P]] = f.succ(n)
    })
  def elimEq[F[_]](f: Nat.ElimEq[F, N]): F[N]
  def n: N
}

object Nat {
  type Zero = Zero.type
  case object Zero
  final case class Succ[P: Nat](p: P)

  def apply[N](implicit nat: Nat[N]): Nat[N] = nat

  trait Elim[F[_]] {
    def zero: F[Zero]
    def succ[N: Nat](n: N): F[Succ[N]]

    def elim[N: Nat]: F[N] = Nat[N].elim(this)
  }

  trait ElimEq[F[_], N] {
    def zero(eq: N ~= Zero): F[Zero]
    def succ[P: Nat](n: P)(eq: N ~= Succ[P]): F[Succ[P]]

    def elim(implicit N: Nat[N]): F[N] = N.elimEq(this)
  }

  implicit val zeroNat: Nat[Zero] = new Nat[Zero] {
    def elimEq[F[_]](f: ElimEq[F, Zero]): F[Zero] = f.zero(Equals.id)
    val n: Zero                                   = Zero
  }

  implicit def succNat[P](implicit p: Nat[P]): Nat[Succ[P]] = new Nat[Succ[P]] {
    def elimEq[F[_]](f: ElimEq[F, Succ[P]]): F[Succ[P]] = f.succ(p.n)(Equals.id)
    val n: Succ[P]                                      = Succ(p.n)
  }
}

sealed abstract class Fin[N](implicit val nat: Nat[N])

object Fin {
  final case class FZero[N: Nat]()             extends Fin[N]
  final case class FSucc[N: Nat](prev: Fin[N]) extends Fin[Succ[N]]

  def top[N: Nat]: Fin[N] = Nat[N].elim(
    new Nat.Elim[Fin] {
      def zero               = FZero()
      def succ[M: Nat](n: M) = FSucc(top[M])
    }
  )

  def bump[N: Nat](fin: Fin[N]): Fin[Succ[N]] =
    fin match {
      case FZero() => FZero()
      case FSucc(prev) =>
        import prev.nat
        FSucc(bump(prev))
    }

  type IsTopRec[n] = Fin[n] => Option[Fin[n]]
  def unbump[N: Nat](fin: Fin[Succ[N]]): Option[Fin[N]] =
    fin match {
      case FZero() => Some(FZero[N]())
      case FSucc(p) =>
        val f = new Nat.Elim[IsTopRec] {
          def zero: Fin[Zero] => Option[Fin[Zero]]                     = _ => None
          def succ[U: Nat](n: U): Fin[Succ[U]] => Option[Fin[Succ[U]]] = p => unbump(p).map(FSucc(_))
        }.elim[N]
        f(p)
    }

  def toInt[N: Nat](fin: Fin[N]): Int = fin match {
    case FZero() => 0
    case FSucc(p) =>
      import p.nat
      toInt(p) + 1
  }

  implicit def order[N: Nat]: Order[Fin[N]] =
    new Nat.Elim[λ[n => Order[Fin[n]]]] {
      def zero = (x, y) => 0
      def succ[P: Nat](n: P) =
        (x, y) =>
          CompareSubst(x, y) match {
            case LT(_) => -1
            case EQ()  => 0
            case GT(_) => 1
        }
    }.elim[N]
}

sealed abstract class CompareSubst[N: Nat] {
  def succ: CompareSubst[Succ[N]]
}

object CompareSubst {
  final case class LT[N: Nat](x: Fin[N]) extends CompareSubst[N] {
    def succ = LT(FSucc(x))
  }
  final case class EQ[N: Nat]() extends CompareSubst[N] {
    def succ = EQ()
  }
  final case class GT[N: Nat](x: Fin[N]) extends CompareSubst[N] {
    def succ = GT(FSucc(x))
  }

  /** comparison of two `Fin[Succ[n]]`
    * if x < y then x should be Fin[N]
    * if x == y then just return it
    * if x > y then x - 1 should be Fin[N]*/
  def apply[N: Nat](x: Fin[Succ[N]], y: Fin[Succ[N]]): CompareSubst[N] =
    (x, y) match {
      case (FZero(), FZero())  => EQ()
      case (FZero(), FSucc(_)) => LT(FZero())
      case (FSucc(p), FZero()) => GT(p)
      case (FSucc(px), FSucc(py)) =>
        new Nat.ElimEq[CompareSubst, N] {
          override def zero(eq: N ~= Zero): CompareSubst[Zero] = EQ()
          override def succ[P: Nat](n: P)(eq: N ~= Succ[P]): CompareSubst[Succ[P]] =
            CompareSubst[P](eq(px), eq(py)).succ
        }.elim
    }
}

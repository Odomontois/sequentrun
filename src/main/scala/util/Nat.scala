package util
import util.Fin.{FSucc, FZero}
import util.Nat.{Succ, Zero}

trait Nat[N] {
  def elim[F[_]](f: Nat.Elim[F]): F[N]
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
  }

  implicit val zeroNat: Nat[Zero] = new Nat[Zero] {
    def elim[F[_]](f: Elim[F]): F[Zero] = f.zero
    val n: Zero                         = Zero
  }

  implicit def succNat[P](implicit p: Nat[P]): Nat[Succ[P]] = new Nat[Succ[P]] {
    def elim[F[_]](f: Elim[F]): F[Succ[P]] = f.succ(p.n)
    val n: Succ[P]                         = Succ(p.n)
  }
}

trait Fin[N, I] {
  def nat: Nat[N]
  def i: I
  def elim[F[_, _]](f: Fin.Elim[F]): F[N, I]
}

object Fin {
  def apply[N, I](implicit fin: Fin[N, I]): Fin[N, I] = fin
  final case class FZero[N: Nat](n: N)
  final case class FSucc[PN: Nat, PI: Fin[PN, ?]](n: PN, p: PI)

  trait Elim[F[_, _]] {
    def zero[N: Nat]: F[N, FZero[N]]
    def succ[PN: Nat, PI: Fin[PN, ?]](n: PN, p: PI): F[Succ[PN], FSucc[PN, PI]]
  }

  implicit def fzero[N: Nat]: Fin[N, FZero[N]] = new Fin[N, FZero[N]] {
    val nat                                       = Nat[N]
    def elim[F[_, _]](f: Elim[F]): F[N, FZero[N]] = f.zero[N]
    val i: FZero[N]                               = FZero(nat.n)
  }

  implicit def fsucc[PN, PI: Fin[PN, ?]]: Fin[Succ[PN], FSucc[PN, PI]] = new Fin[Succ[PN], FSucc[PN, PI]] {
    val pi: Fin[PN, PI]                                       = Fin[PN, PI]
    implicit val pnat                                         = pi.nat
    val nat                                                   = Nat[Succ[PN]]
    def elim[F[_, _]](f: Elim[F]): F[Succ[PN], FSucc[PN, PI]] = f.succ(pnat.n, Fin[PN, PI].i)
    val i                                                     = FSucc(pnat.n, Fin[PN, PI].i)
  }
}

trait BumpFin[N, I] {
  type Out
  def fin: Fin[Succ[N], Out]
}

object BumpFin {
  type Aux[N, I, O] = BumpFin[N, I] { type Out = O }

  implicit def bumpZero[N: Nat]: Aux[N, FZero[N], FZero[Succ[N]]] = new BumpFin[N, FZero[N]] {
    type Out = FZero[Succ[N]]
    val fin = Fin[Succ[N], FZero[Succ[N]]]
  }

  implicit def bumpSucc[N, I: Fin[N, ?]](
      implicit bump: BumpFin[N, I]): Aux[Succ[N], FSucc[N, I], FSucc[Succ[N], bump.Out]] =
    new BumpFin[Succ[N], FSucc[N, I]] {
      type Instance[n, a] = Fin[Succ[n], FSucc[n, a]]
      type Out            = FSucc[Succ[N], bump.Out]
      val fin: Instance[Succ[N], bump.Out] = bump.fin.elim[Instance] {
        implicit val prevFin = bump.fin

        new Fin.Elim[Instance] {
          def zero[U: Nat]: Instance[U, FZero[U]] = Fin[Succ[U], FSucc[U, FZero[U]]]
          def succ[PN: Nat, PI: Fin[PN, ?]](n: PN, p: PI): Instance[Succ[PN], FSucc[PN, PI]] =
            Fin[Succ[Succ[PN]], FSucc[Succ[PN], FSucc[PN, PI]]]
        }
      }
    }
}

trait TopFin[N] {
  type Out
  def fin: Fin[N, Out]
}

object TopFin {
  type Aux[N, O] = TopFin[N] { type Out = O }

  implicit val topZero: Aux[Zero, FZero[Zero]] = new TopFin[Zero] {
    type Out = FZero[Zero]
    val fin = Fin[Zero, FZero[Zero]]
  }

  implicit def topSucc[N: Nat](implicit top: TopFin[N]): Aux[Succ[N], FSucc[N, top.Out]] =
    new TopFin[Succ[N]] {
      type Out            = FSucc[N, top.Out]
      private implicit val prevFin = top.fin
      val fin = Fin[Succ[N], FSucc[N, top.Out]]
    }
}

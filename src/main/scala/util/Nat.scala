package util
import util.Nat.{NatElim, Succ, Zero}

sealed trait Nat {
  type Self >: this.type <: Nat
  def elim[F[_ <: Nat]](elim: NatElim[F]): F[Self]
}

object Nat {
  type Zero = Zero.type
  case object Zero extends Nat {
    type Self = Zero
    def elim[F[_ <: Nat]](elim: NatElim[F]): F[Zero] = elim.zero
  }
  final case class Succ[N <: Nat](p: N)(implicit val reg: RegularNat[N]) extends Nat {
    type Self = Succ[N]
    def elim[F[_ <: Nat]](elim: NatElim[F]): F[Succ[N]] = elim.succ(p)
  }

  trait NatElim[F[_ <: Nat]] {
    def zero: F[Zero]
    def succ[N <: Nat](n: N)(implicit reg: RegularNat[N]): F[Succ[N]]
  }
}

trait RegularNat[N <: Nat] {
  def regularize[F[_ <: Nat]](n: N)(f: F[n.Self]): F[N]
}

object RegularNat {
  implicit def nilReg: RegularNat[Zero] = new RegularNat[Zero] {
    def regularize[F[_ <: Nat]](n: Zero)(f: F[Zero]): F[Zero] = f
  }

  implicit def succReg[N <: Nat](implicit reg: RegularNat[N]): RegularNat[Succ[N]] = new RegularNat[Succ[N]] {
    def regularize[F[_ <: Nat]](n: Succ[N])(f: F[Succ[N]]): F[Succ[N]] = f
  }
}

sealed trait Fin[N <: Nat] {
  def n: N
  def bump: Fin[Succ[N]]
}

object Fin {
  final case class FZero[N <: Nat : RegularNat](n: N) extends Fin[N] {
    def bump: Fin[Succ[N]] = FZero(Succ(n))
  }
  final case class FSucc[N <: Nat : RegularNat](p: Fin[N], n: Succ[N]) extends Fin[Succ[N]] {
    def bump: Fin[Succ[Succ[N]]] = FSucc(p.bump, Succ(n))
  }

  def top[N <: Nat](n: N)(implicit reg: RegularNat[N]): Fin[N] =
    reg.regularize[Fin](n)(
      n.elim[Fin] {
        new NatElim[Fin] {
          def zero: Fin[Zero] = FZero(Zero)
          def succ[M <: Nat : RegularNat](m: M): Fin[Succ[M]] = FSucc(top[M](m), Succ(m))
        }
      })
}

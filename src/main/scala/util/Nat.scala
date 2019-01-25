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
    val n: Zero = Zero
  }

  implicit def succNat[P](implicit p: Nat[P]): Nat[Succ[P]] = new Nat[Succ[P]] {
    def elim[F[_]](f: Elim[F]): F[Succ[P]] = f.succ(p.n)
    val n: Succ[P] = Succ(p.n)
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
    val nat = Nat[N]
    def elim[F[_, _]](f: Elim[F]): F[N, FZero[N]] = f.zero[N]
    val i: FZero[N] = FZero(nat.n)
  }

  implicit def fsucc[PN, PI: Fin[PN, ?]]: Fin[Succ[PN], FSucc[PN, PI]] = new Fin[Succ[PN], FSucc[PN, PI]] {
    val pi: Fin[PN, PI] = Fin[PN, PI]
    implicit val pnat = pi.nat
    val nat = Nat[Succ[PN]]
    def elim[F[_, _]](f: Elim[F]): F[Succ[PN], FSucc[PN, PI]] = f.succ(pnat.n, Fin[PN, PI].i)
    val i = FSucc(pnat.n, Fin[PN, PI].i)
  }
}


trait SomeFin[N] {
  type Out
  implicit def fin: Fin[N, Out]
}

object SomeFin {
  def apply[N, I](implicit f: Fin[N, I]): SomeFin[N] = new SomeFin[N] {
    type Out = I
    val fin: Fin[N, I] = f
  }
}

object FinBump {
  type Rec[n, i] = SomeFin[Succ[n]]
  def apply[N: Nat, I: Fin[N, ?]]: SomeFin[Succ[N]] =
    Fin[N, I].elim[Rec](new Fin.Elim[Rec] {
      def zero[U: Nat]: SomeFin[Succ[U]] = SomeFin[Succ[U], FZero[Succ[U]]]
      def succ[PN: Nat, PI: Fin[PN, ?]](n: PN, p: PI): SomeFin[Succ[Succ[PN]]] = {
        val pbump = FinBump[PN, PI]
        implicit val pfin = pbump.fin
        SomeFin[Succ[Succ[PN]], FSucc[Succ[PN], pbump.Out]]
      }
    })
}

object TopFin {
  def apply[N: Nat]: SomeFin[N] = Nat[N].elim(new Nat.Elim[SomeFin] {
    def zero: SomeFin[Zero] = SomeFin[Zero, FZero[Zero]]
    def succ[U: Nat](n: U): SomeFin[Succ[U]] = {
      val ptop = TopFin[U]
      implicit val pfin = ptop.fin
      SomeFin[Succ[U], FSucc[U, ptop.Out]]
    }
  })

}

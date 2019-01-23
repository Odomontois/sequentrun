package util
import util.Nat.Succ

sealed trait Nat

object Nat {
  type Zero = Zero.type
  case object Zero extends Nat
  final case class Succ[N <: Nat](p: N) extends Nat
}

sealed trait Fin[N <: Nat.Succ[_]] {
  def bump: Fin[Succ[N]]
}

object Fin {
  final case class FZero[N <: Succ[_]]() extends Fin[N] {
    def bump: Fin[Succ[N]] = FZero()
  }
  final case class FSucc[N <: Succ[_]](p: Fin[N]) extends Fin[Succ[N]] {
    def bump: Fin[Succ[Succ[N]]] = FSucc(p.bump)
  }
}
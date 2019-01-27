package util
import util.Nat.{Succ, Zero}

trait Nat[N] {
  def elim[F[_]](f: Nat.Elim[F]): F[N] =
    elimEq(new Nat.ElimEq[F, N] {
      def zero(eq: N =~= Zero): F[Zero]                     = f.zero
      def succ[P: Nat](n: P)(eq: N =~= Succ[P]): F[Succ[P]] = f.succ(n)
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
  }

  trait ElimEq[F[_], N] {
    def zero(eq: N =~= Zero): F[Zero]
    def succ[P: Nat](n: P)(eq: N =~= Succ[P]): F[Succ[P]]
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
        val f = Nat[N].elim[IsTopRec](
          new Nat.Elim[IsTopRec] {
            def zero: Fin[Zero] => Option[Fin[Zero]]                     = _ => None
            def succ[U: Nat](n: U): Fin[Succ[U]] => Option[Fin[Succ[U]]] = p => unbump(p).map(FSucc(_))
          }
        )
        f(p)
    }
}

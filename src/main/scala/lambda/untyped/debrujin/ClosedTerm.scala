package lambda.untyped.debrujin
import util.Fin.{FSucc, FZero}
import util.{Fin, FinBump, Nat}
import util.Nat.Succ

abstract class ClosedTerm[N: Nat] {
  def bump: ClosedTerm[Succ[N]]
  //  def stepBN: Option[ClosedTerm[N]]
}

object ClosedTerm {
    def subst[N : Nat](t: ClosedTerm[Succ[N]])(sub: ClosedTerm[N]): ClosedTerm[N] =
      t match {
        case Lam(body) => Lam(subst[Succ[N]](body)(sub.bump))
        case App(f, x) => App(subst(f)(sub), subst(x)(sub))
        case Var() =>
      }

  case class Lam[N: Nat](term: ClosedTerm[Succ[N]]) extends ClosedTerm[N] {
    def bump: ClosedTerm[Succ[N]] = Lam(term.bump)
    //    def stepBN: Option[ClosedTerm[N]] = term.stepBN.map(Lam(_))
  }
  case class App[N: Nat](f: ClosedTerm[N], x: ClosedTerm[N]) extends ClosedTerm[N] {
    def bump: ClosedTerm[Succ[N]] = App(f.bump, x.bump)
    def stepBN: Option[ClosedTerm[N]] = f match {
      case Lam(t) => ??? // Some(subst(t)(x))
      //      case Vr(_) => x.stepBN.map(Ap(f, _))
      //      case _ => f.stepBN.map(Ap(_, x))
    }
  }
  case class Var[N: Nat, I: Fin[N, ?]]() extends ClosedTerm[N]()(Fin[N, I].nat) {
    def bump: ClosedTerm[Succ[N]] = {
      val bumped = FinBump[N, I]
      import bumped.fin
      Var[Succ[N], bumped.Out]
    }
    def substWith(t: Term)
    //    def stepBN: Option[ClosedTerm[N]] = None
  }
}

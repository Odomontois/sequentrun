package lambda.untyped.debrujin
import util.{Fin, Nat}
import util.Nat.Succ

abstract class ClosedTerm[N: Nat] {
  def bump: ClosedTerm[Succ[N]]
  def stepBN: Option[ClosedTerm[N]]
}

object ClosedTerm {
  def subst[N: Nat](t: ClosedTerm[Succ[N]])(sub: ClosedTerm[N]): ClosedTerm[N] =
    t match {
      case Lam(body) => Lam(subst[Succ[N]](body)(sub.bump))
      case App(f, x) => App(subst(f)(sub), subst(x)(sub))
      case Var(idx)  => Fin.unbump[N](idx).fold(sub)(Var(_))
    }

  case class Lam[N: Nat](term: ClosedTerm[Succ[N]]) extends ClosedTerm[N] {
    def bump: ClosedTerm[Succ[N]]     = Lam(term.bump)
    def stepBN: Option[ClosedTerm[N]] = term.stepBN.map(Lam(_))
  }
  case class App[N: Nat](f: ClosedTerm[N], x: ClosedTerm[N]) extends ClosedTerm[N] {
    def bump: ClosedTerm[Succ[N]] = App(f.bump, x.bump)
    def stepBN: Option[ClosedTerm[N]] = f match {
      case Lam(t) => Some(subst(t)(x))
      case Var(_) => x.stepBN.map(App(f, _))
      case _      => f.stepBN.map(App(_, x))
    }
  }

  def mkVar[N](f: Fin[N]): Var[N] = {
    import f.nat
    Var(f)
  }
  case class Var[N: Nat](fin: Fin[N]) extends ClosedTerm[N]() {
    def bump: ClosedTerm[Succ[N]]     = mkVar[Succ[N]](Fin.bump(fin))
    def stepBN: Option[ClosedTerm[N]] = None
  }
}

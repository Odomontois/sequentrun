package lambda.untyped.debrujin
import lambda.untyped.debrujin.CompareSubst.{EQ, GT, LT}
import util.Fin.{FSucc, FZero}
import util.{=~=, Fin, Nat}
import util.Nat.{Succ, Zero}

abstract class Closed[N: Nat] {
  def bump: Closed[Succ[N]]
  def stepBN: Option[Closed[N]]
  def isVar: Boolean
}

object Closed {
  def subst[N: Nat](t: Closed[Succ[N]], i: Fin[N])(sub: Closed[N]): Closed[N] =
    t match {
      case Lam(body) => Lam(subst[Succ[N]](body, Fin.bump(i))(sub.bump))
      case App(f, x) => App(subst(f, i)(sub), subst(x, i)(sub))
      case Var(idx) =>
        Nat[N].elimEq(new Nat.ElimEq[Closed, N] {
          override def zero(eq: N =~= Zero): Closed[Zero] = eq.subst[Closed](sub)
          override def succ[P: Nat](n: P)(eq: N =~= Succ[P]): Closed[Succ[P]] =
            CompareSubst(eq.subst(idx), eq.subst(i)) match {
              case LT(p) => Var(p)
              case EQ()  => eq.subst(sub)
              case GT(p) => Var(p)
            }
        })
    }

  case class Lam[N: Nat](term: Closed[Succ[N]]) extends Closed[N] {
    def bump: Closed[Succ[N]]     = Lam(term.bump)
    def stepBN: Option[Closed[N]] = term.stepBN.map(Lam(_))
    def isVar                     = false
  }

  case class App[N: Nat](f: Closed[N], x: Closed[N]) extends Closed[N] {
    def bump: Closed[Succ[N]] = App(f.bump, x.bump)
    def stepBN: Option[Closed[N]] = f match {
      case Lam(t)       => Some(subst(t, FZero())(x))
      case t if t.isVar => x.stepBN.map(App(f, _))
      case _            => f.stepBN.map(App(_, x))
    }
    def isVar = false
  }

  def mkVar[N](f: Fin[N]): Var[N] = {
    import f.nat
    Var(f)
  }

  case class Var[N: Nat](fin: Fin[N]) extends Closed[Succ[N]]() {
    def bump: Closed[Succ[Succ[N]]]     = mkVar[Succ[N]](Fin.bump(fin))
    def stepBN: Option[Closed[Succ[N]]] = None
    def isVar                           = true
  }

  def v0[N: Nat]: Closed[Succ[N]]                                       = Var(FZero())
  def v1[N: Nat]: Closed[Succ[Succ[N]]]                                 = Var(FSucc(FZero()))
  def v2[N: Nat]: Closed[Succ[Succ[Succ[N]]]]                           = Var(FSucc(FSucc(FZero())))
  def v3[N: Nat]: Closed[Succ[Succ[Succ[Succ[N]]]]]                     = Var(FSucc(FSucc(FSucc(FZero()))))
  def app2[N: Nat](f: Closed[N], x: Closed[N], y: Closed[N]): Closed[N] = App(App(f, x), y)
  def lam2[N: Nat](f: Closed[Succ[Succ[N]]]): Closed[N]                 = Lam(Lam(f))
  def lam3[N: Nat](f: Closed[Succ[Succ[Succ[N]]]]): Closed[N]           = Lam(lam2(f))

  def True[N: Nat]: Closed[N]  = lam2(v1)
  def False[N: Nat]: Closed[N] = lam2(v0)

  def C0[N: Nat]: Closed[N]    = lam2(v0)
  def Csucc[N: Nat]: Closed[N] = lam3(App(v1, app2(v2, v1, v0)))

  def Cadd[N: Nat]: Closed[N] = lam2(app2(v0, Csucc, v1))
  def Cmul[N: Nat]: Closed[N] = lam2(app2(v0, App(Cadd, v1), C0))
}

sealed abstract class CompareSubst[N: Nat] {
  def bump: CompareSubst[Succ[N]]
}

object CompareSubst {
  final case class LT[N: Nat](x: Fin[N]) extends CompareSubst[N] {
    def bump = LT(Fin.bump(x))
  }
  final case class EQ[N: Nat]() extends CompareSubst[N] {
    def bump = EQ()
  }
  final case class GT[N: Nat](x: Fin[N]) extends CompareSubst[N] {
    def bump = GT(Fin.bump(x))
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
        Nat[N].elimEq(new Nat.ElimEq[CompareSubst, N] {
          override def zero(eq: N =~= Zero): CompareSubst[Zero] = EQ()
          override def succ[P: Nat](n: P)(eq: N =~= Succ[P]): CompareSubst[Succ[P]] =
            CompareSubst[P](eq.subst(px), eq.subst(py)).bump
        })
    }
}

package lambda.untyped.debrujin
import cats.Show
import cats.syntax.show._
import cats.syntax.order._
import cats.instances.int.catsStdShowForInt
import util.CompareSubst.{EQ, GT, LT}
import util.Fin.{FSucc, FZero}
import util._
import util.Nat.{Succ, Zero}

abstract class Closed[N: Nat] {
  def bumpOver(idx: Fin[N]): Closed[Succ[N]]
  def stepBN: Option[Closed[N]]
  def reducedBN: Closed[N] = stepBN match {
    case None    => this
    case Some(t) => t.reducedBN
  }
  def isVar: Boolean
}

object Closed {
  def subst[N: Nat](t: Closed[Succ[N]], i: Fin[N])(sub: Closed[N]): Closed[N] =
    t match {
      case Lam(body) => Lam(subst[Succ[N]](body, FSucc(i))(sub.bumpOver(FZero())))
      case App(f, x) => App(subst(f, i)(sub), subst(x, i)(sub))
      case Var(idx) =>
        Nat[N].elimEq(new Nat.ElimEq[Closed, N] {
          override def zero(eq: N =~= Zero): Closed[Zero] = eq(sub)
          override def succ[P: Nat](n: P)(eq: N =~= Succ[P]): Closed[Succ[P]] =
            CompareSubst(eq.apply(idx), eq.apply(i)) match {
              case LT(p) => Var(p)
              case EQ()  => eq(sub)
              case GT(p) => Var(p)
            }
        })
    }

  case class Lam[N: Nat](term: Closed[Succ[N]]) extends Closed[N] {
    def bumpOver(idx: Fin[N]): Closed[Succ[N]] = Lam(term.bumpOver(FSucc(idx)))
    def stepBN: Option[Closed[N]]              = term.stepBN.map(Lam(_))
    def isVar                                  = false
  }

  case class App[N: Nat](f: Closed[N], x: Closed[N]) extends Closed[N] {
    def bumpOver(idx: Fin[N]): Closed[Succ[N]] = App(f.bumpOver(idx), x.bumpOver(idx))
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

  case class Var[N: Nat](i: Fin[N]) extends Closed[Succ[N]]() {
    def bumpOver(idx: Fin[Succ[N]]): Closed[Succ[Succ[N]]] =
      idx match {
        case FZero()     => mkVar(FSucc(i))
        case FSucc(pidx) => if (i <= pidx) mkVar(Fin.bump(i)) else mkVar(FSucc(i))
      }
    def stepBN: Option[Closed[Succ[N]]] = None
    def isVar                           = true
  }

  def reduce(t: Closed[Zero]): Closed[Zero] = t.reducedBN

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
  def C1[N: Nat]: Closed[N]    = lam2(App(v1, v0))
  def C2[N: Nat]: Closed[N]    = lam2(App(v1, App(v1, v0)))
  def Csucc[N: Nat]: Closed[N] = lam3(App(v1, app2(v2, v1, v0)))

  def Cadd[N: Nat]: Closed[N] = lam2(app2(v0, Csucc, v1))
  def Cmul[N: Nat]: Closed[N] = lam2(app2(v0, App(Cadd, v1), C0))
  def Cpow[N: Nat]: Closed[N] = lam2(app2(v0, App(Cmul, v1), C1))

  implicit def show[N: Nat]: Show[Closed[N]] = {
    case Lam(t)    => show"λ.$t"
    case App(f, x) => show"($f $x)"
    case Var(i) =>
      import i.nat
      show"${Fin.toInt(i)}"
  }
}

object ClosedApp extends App {
  import Closed._
  def prnt(t: Closed[Zero]): Unit = println(t.show)

//  prnt(C0)
//  prnt(App(Csucc, C0))
//  prnt(App[Zero](Csucc, C0).reducedBN)
//  prnt(C2)
  iter(app2[Zero](Cadd, C1, C1))(_.stepBN).steps.map(_.show).foreach(println)
  iter(app2[Zero](Cadd, C2, C2))(_.stepBN).steps.map(_.show).foreach(println)
  iter(app2[Zero](Cmul, C2, C2))(_.stepBN).steps.map(_.show).foreach(println)
//  iter[Closed[Succ[Zero]]](App(Lam(App(v1, v0)), v0))(_.stepBN).steps.map(_.show).foreach(println)
//  println(subst[Succ[Zero]](App(v1, v0), FZero())(v0))
//  CompareSubst[]
}

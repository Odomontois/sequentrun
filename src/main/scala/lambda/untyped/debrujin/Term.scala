package lambda.untyped.debrujin

import lambda.untyped.debrujin.Term.{Ap, La, Vr}
import lambda.untyped.debrujin.terms.{app, lam, vr}

sealed trait Term {
  def subst(level: Int, term: Term): Term
  def stepBN: Option[Term]
  def reduceBN: Term = stepBN match {
    case None => this
    case Some(term) => term.reduceBN
  }
  def to[A: Lam]: A
}

object Term extends TermInstance {
  final case class La(term: Term) extends Term {
    def subst(level: Int, t: Term) = La(term.subst(level + 1, t))
    def isVarApp = false

    def to[A: Lam]: A = lam(term.to[A])
    def stepBN: Option[Term] = term.stepBN.map(La)
  }
  final case class Ap(f: Term, x: Term) extends Term {
    def subst(level: Int, term: Term) =
      Ap(f.subst(level, term), x.subst(level, term))
    def to[A: Lam]: A = app(f.to[A], x.to[A])
    def stepBN: Option[Term] = f match {
      case La(t) => Some(t.subst(1, x))
      case Vr(_) => x.stepBN.map(Ap(f, _))
      case _ => f.stepBN.map(Ap(_, x))
    }
  }
  final case class Vr(v: Int) extends Term {
    def subst(level: Int, term: Term): Term =
      if (level == v) term else this
    def to[A: Lam]: A = vr[A](v)
    def stepBN: Option[Term] = None
  }
}
class TermInstance {
  final implicit object freeze extends Lam[Term] {
    def lam(term: Term): Term = La(term)
    def app(f: Term, x: Term): Term = Ap(f, x)
    def v(Int: Int): Term = Vr(Int)
  }
}

package lambda.untyped

import lambda.untyped.terms.{app, lam, vr}
import lambda.untyped.Lam.Name
import lambda.untyped.Term.{Ap, La, Vr}

sealed trait Term {
  def free: Set[Name]
  def subst(name: Name, term: Term) =
    if (free(name)) subst1(name, term) else this
  protected def subst1(name: Name, term: Term): Term
  def stepBN: Option[Term]
  def reduceBN: Term = stepBN match {
    case None => this
    case Some(term) => term.reduceBN
  }
  def to[A: Lam]: A

  def fresh: Name
  def isVal: Boolean
}

object Term extends TermInstance {
  final case class La(vname: Name, term: Term) extends Term {
    lazy val free: Set[Name] = term.free - vname
    def subst1(name: Name, t: Term) =
      if (t.free(vname)) {
        val vn1 = term.fresh max t.fresh
        La(vn1, term.subst(vname, Vr(vn1)).subst(name, t))
      } else La(vname, term.subst(name, t))
    def isVarApp = false

    def to[A: Lam]: A = lam(vname, term.to[A])
    def fresh: Name = term.fresh
    def stepBN: Option[Term] = term.stepBN.map(La(vname, _))
    def isVal: Boolean = false
  }
  final case class Ap(f: Term, x: Term) extends Term {
    lazy val free = f.free | x.free
    def subst1(name: Name, term: Term) =
      Ap(f.subst(name, term), x.subst(name, term))
    def to[A: Lam]: A = app(f.to[A], x.to[A])
    def fresh: Name = f.fresh max x.fresh
    def stepBN: Option[Term] = f match {
      case La(name, t) => Some(t.subst(name, x))
      case _ if f.isVal => x.stepBN.map(Ap(f, _))
      case _ => f.stepBN.map(Ap(_, x))
    }
    def isVal: Boolean = false
  }
  final case class Vr(vname: Name) extends Term {
    def free: Set[Name] = Set(vname)
    protected def subst1(name: Name, term: Term): Term = term
    def to[A: Lam]: A = vr[A](vname)
    def fresh: Name = vname + 1
    def stepBN: Option[Term] = None
    def isVal: Boolean = true
  }
}
class TermInstance {
  final implicit object freeze extends Lam[Term] {
    def lam(name: Name, term: Term): Term = La(name, term)
    def app(f: Term, x: Term): Term = Ap(f, x)
    def v(name: Name): Term = Vr(name)
  }
}

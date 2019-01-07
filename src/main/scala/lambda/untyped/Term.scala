package lambda.untyped

import lambda.untyped.terms.{app, lam, v}
import lambda.untyped.Lam.Name
import lambda.untyped.Term.{Ap, La, Vr}

sealed trait Term {
  def free: Set[Name]
  def normal: Boolean
  def subst(name: Name, term: Term) =
    if (free(name)) subst1(name, term) else this
  protected def subst1(name: Name, term: Term): Term
  def isVarApp: Boolean

  def smallStepBN: Term

  def reduceBN: Term = if (normal) this else smallStepBN.reduceBN

  def to[A: Lam]: A

  def fresh: Name
}

object Term extends TermInstance {
  final case class La(vname: Name, term: Term) extends Term {
    lazy val free: Set[Name] = term.free - vname
    lazy val normal: Boolean = term.normal
    def subst1(name: Name, t: Term) =
      if (t.free(vname)) {
        val vn1 = term.fresh max t.fresh
        La(vn1, term.subst(vname, Vr(vn1)).subst(name, t))
      } else La(vname, term.subst(name, t))
    def isVarApp = false

    def smallStepBN: Term = La(vname, term.smallStepBN)
    def to[A: Lam]: A = lam(vname, term.to[A])
    def fresh: Name = term.fresh
  }
  final case class Ap(f: Term, x: Term) extends Term {
    lazy val free = f.free | x.free
    lazy val normal = f.isVarApp && x.normal
    def subst1(name: Name, term: Term) =
      Ap(f.subst(name, term), x.subst(name, term))
    def isVarApp = f.isVarApp
    def smallStepBN: Term = f match {
      case _ if !f.normal => Ap(f.smallStepBN, x)
      case La(name, t) => t.subst(name, x)
      case _ => Ap(f, x.smallStepBN)
    }
    def to[A: Lam]: A = app(f.to[A], x.to[A])
    def fresh: Name = f.fresh max x.fresh
  }
  final case class Vr(vname: Name) extends Term {
    def free: Set[Name] = Set(vname)
    def normal: Boolean = true
    protected def subst1(name: Name, term: Term): Term = term
    def isVarApp = true
    def smallStepBN: Term = this
    def to[A: Lam]: A = v[A](vname)
    def fresh: Name = vname + 1
  }
}
class TermInstance {
  final implicit object freeze extends Lam[Term] {
    def lam(name: Name, term: Term): Term = La(name, term)
    def app(f: Term, x: Term): Term = Ap(f, x)
    def v(name: Name): Term = Vr(name)
  }
}

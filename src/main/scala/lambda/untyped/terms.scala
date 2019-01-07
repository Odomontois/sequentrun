package lambda.untyped

import lambda.untyped.Lam.Name

object terms {
  val Seq(x, y, z, a, b, c, d, e, f, g, h, i, rest@_ *) = Stream.from(0).map(Builder)
  val Seq(j, k, l, m, o, p, q, r, s, t, u, v, w, _*) = rest

  final case class Builder(idx: Name) extends AnyVal {
    def v[A: Lam]: A = terms.vr(idx)
    def lam[A: Lam](term: A): A = terms.lam(idx, term)
    def app[A: Lam](term: A): A = terms.app(v, term)
  }

  def abst[A: Lam](f: A => A): A = a.lam(f(a.v))
  def abst2[A: Lam](f: (A, A) => A): A = a.lam(b.lam(f(a.v, b.v)))


  def lam[A](name: Name, term: A)(implicit lam: Lam[A]): A = lam.lam(name, term)
  def app[A](f: A, x: A)(implicit lam: Lam[A]): A = lam.app(f, x)
  def app2[A: Lam](f: A, x: A, y: A): A = app(app(f, x), y)
  def app3[A: Lam](f: A, x: A, y: A, z: A): A = app(app2(f, x, y), z)
  def app4[A: Lam](f: A, a: A, b: A, c: A, d: A): A = app2(app2(f, a, b), c, d)
  def vr[A](name: Name)(implicit lam: Lam[A]): A = lam.v(name)

  def id[A: Lam]: A = x.lam(x.v)
  def True[A: Lam]: A = x.lam(y.lam(x.v))
  def False[A: Lam]: A = x.lam(y.lam(y.v))

  def bnot[A: Lam](x: A): A = app2(x, False, True)
  def not[A: Lam]: A = abst(bnot[A])

  def band[A: Lam](x: A, y: A): A = app2(x, y, False)

  def and[A: Lam]: A = abst2(band[A])
  def bor[A: Lam](x: A, y: A): A = app2(x, True, y)

  def or[A: Lam]: A = abst2(bor[A])

  def zero[A: Lam]: A = x.lam(y.lam(y.v))
  def one[A: Lam]: A = x.lam(y.lam(x.app(y.v)))
  def two[A: Lam]: A = x.lam(y.lam(x.app(x.app(y.v))))
  def three[A: Lam]: A = x.lam(y.lam(x.app(x.app(x.app(y.v)))))
  def four[A: Lam]: A = x.lam(y.lam(x.app(x.app(x.app(x.app(y.v))))))
  def five[A: Lam]: A = x.lam(y.lam(x.app(x.app(x.app(x.app(x.app(y.v)))))))

  def succ[A: Lam](n: A): A = s.lam(z.lam(s.app(app2(n, s.v, z.v))))
  def plus[A: Lam](a: A, b: A): A = x.lam(y.lam(app2(a, x.v, app2(b, x.v, y.v))))
  def add[A: Lam]: A = abst2(plus[A])
  def times[A: Lam](a: A, b: A): A = app2(a, app(add, b), zero)
  def mul[A: Lam]: A = abst2(times[A])
  def exp[A: Lam](a: A, b: A): A = app2(b, app(mul, a), one)
  def pow[A: Lam]: A = abst2(exp[A])

  def fix[A: Lam]: A = f.lam(app(x.lam(app(f.v, app(x.v, x.v))), x.lam(app(f.v, app(x.v, x.v)))))

  def pair[A: Lam](f: A, s: A): A = z.lam(app2(z.v, f, s))
  def first[A: Lam](p: A): A = app(p, True)
  def fst[A: Lam]: A = y.lam(first(y.v))
  def second[A: Lam](p: A): A = app(p, False)
  def snd[A: Lam]: A = y.lam(second(y.v))

  def Ф[A: Lam]: A = p.lam(z.lam(app2(z.v, succ(first(p.v)), first(p.v))))
  def prednat[A: Lam](x: A): A = second(app2(x, Ф, pair(zero, zero)))
  def pred[A: Lam]: A = abst(prednat[A])

  def minus[A: Lam](x: A, y: A): A = app2(y, pred, x)
  def substract[A: Lam]: A = abst2(minus[A])

  def iszero[A: Lam](n: A) = app2(n, x.lam(False), True)
  def is0[A: Lam]: A = abst[A](iszero)

  def natle[A: Lam](x: A, y: A) = iszero(minus(x, y))
  def le[A: Lam] = abst2(natle[A])

  def natge[A: Lam](x: A, y: A) = iszero(minus(y, x))
  def ge[A: Lam]: A = abst2(natge[A])

  def natlt[A: Lam](x: A, y: A) = bnot(natge(x, y))
  def lt[A: Lam]: A = abst2(natlt[A])

  def natgt[A: Lam](x: A, y: A) = bnot(natle(x, y))
  def gt[A: Lam]: A = abst2(natgt[A])

  def nateq[A: Lam](x: A, y: A) = band(natle(x, y), natge(x, y))
  def eqn[A: Lam] = abst2(nateq[A])

  def fact[A: Lam] = app(fix, r.lam(x.lam(app2(iszero(x.v), one, app2(mul, app(r.v, prednat(x.v)), x.v)))))
}
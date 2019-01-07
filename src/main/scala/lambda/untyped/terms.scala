package lambda.untyped

import lambda.untyped.Lam.Name

object terms {
  val Seq(x, y, z, a, b, c, d, e, f, g, h, i, rest@_ *) = Stream.from(0).map(Builder)
  val Seq(j, k, l, m, n, o, p, q, r, s, t, u, v, w, _*) = rest

  final case class Builder(idx: Name) extends AnyVal {
    def v[A: Lam]: A = terms.vr(idx)
    def lam[A: Lam](term: A): A = terms.lam(idx, term)
    def ^[A: Lam](term: A): A = lam(term)
    def app[A: Lam](term: A): A = terms.app(v, term)
    def ![A: Lam](term: A): A = app(term)
    def ![A: Lam](x: A, y: A): A = terms.app2(v, x, y)
  }

  implicit class LamApp[A](val a: A) extends AnyVal {
    def !(x: A)(implicit A: Lam[A]): A = app(a, x)
    def !(x: A, y: A)(implicit A: Lam[A]): A = app2(a, x, y)
  }

  implicit class Tuple2Abs(val p: (Builder, Builder)) extends AnyVal {
    def ^[A: Lam](x: A): A = p._1 ^ (p._2 ^ x)
  }

  implicit class Tuple3Abs(val p: (Builder, Builder, Builder)) extends AnyVal {
    def ^[A: Lam](x: A): A = p._1 ^ (p._2 ^ (p._3 ^ x))
  }

  implicit class Tuple4Abs(val p: (Builder, Builder, Builder, Builder)) extends AnyVal {
    def ^[A: Lam](x: A): A = p._1 ^ (p._2 ^ (p._3 ^ (p._4 ^ x)))
  }

  def lam[A](name: Name, term: A)(implicit lam: Lam[A]): A = lam.lam(name, term)
  def app[A](f: A, x: A)(implicit lam: Lam[A]): A = lam.app(f, x)
  def app2[A: Lam](f: A, x: A, y: A): A = app(app(f, x), y)
  def vr[A](name: Name)(implicit lam: Lam[A]): A = lam.v(name)

  def id[A: Lam]: A = x.lam(x.v)
  def True[A: Lam]: A = x.lam(y.lam(x.v))
  def False[A: Lam]: A = x.lam(y.lam(y.v))

  def not[A: Lam]: A = x ^ (x ! (False, True))
  def and[A: Lam]: A = (x, y) ^ (x ! (y.v, False))
  def or[A: Lam]: A = (x, y) ^ (x ! (True, y.v))

  def zero[A: Lam]: A = (s, z) ^ z.v
  def one[A: Lam]: A = (s, z) ^ (s ! z.v)
  def two[A: Lam]: A = (s, z) ^ (s ! (s ! z.v))
  def three[A: Lam]: A = (s, z) ^ (s ! (s ! (s ! z.v)))
  def four[A: Lam]: A = (s, z) ^ (s ! (s ! (s ! (s ! z.v))))
  def five[A: Lam]: A = (s, z) ^ (s ! (s ! (s ! (s ! (s ! z.v)))))

  def succ[A: Lam]: A = (n, s, z) ^ (s ! (n ! (s.v, z.v)))
  def plus[A: Lam]: A = (a, b, s, z) ^ (a ! (s.v, b ! (s.v, z.v)))
  def times[A: Lam]: A = (a, b) ^ (a ! (plus ! b.v, zero))
  def pow[A: Lam]: A = (a, b) ^ (b ! (times ! a.v, one))

  def fix[A: Lam]: A = f ^ ((x ^ (f ! (x ! x.v))) ! (x ^ (f ! (x ! x.v))))

  def pair[A: Lam]: A = (f, s, p) ^ (p ! (f.v, s.v))
  def first[A: Lam]: A = p ^ (p ! True)
  def second[A: Lam]: A = p ^ (p ! False)
  def Ф[A: Lam]: A = (p, z) ^ (z ! (succ ! (first ! p.v), first ! p.v))
  def pred[A: Lam]: A = x ^ (second ! (x ! (Ф, pair ! (zero, zero))))

  def minus[A: Lam]: A = (x, y) ^ (y ! (pred, x.v))
  def is0[A: Lam] = n ^ (n ! (x ^ False, True))

  def le[A: Lam] = (x, y) ^ (is0 ! (minus ! (x.v, y.v)))
  def ge[A: Lam] = (x, y) ^ (is0 ! (minus ! (y.v, x.v)))
  def lt[A: Lam] = (x, y) ^ (not ! (ge ! (x.v, y.v)))

  def gt[A: Lam] = (x, y) ^ (not ! (le ! (x.v, y.v)))

  def eqn[A: Lam] = (x, y) ^ (and ! (le ! (x.v, y.v), ge ! (x.v, y.v)))

  def fact[A: Lam] = fix ! ((r, x) ^ ((is0 ! x.v) ! (one, times ! (r ! (pred ! x.v), x.v))))
}
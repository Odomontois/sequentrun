package lambda.untyped.debrujin

object terms {
  implicit class LamApp[A](val a: A) extends AnyVal {
    def !(x: A)(implicit A: Lam[A]): A = app(a, x)
    def !(x: A, y: A)(implicit A: Lam[A]): A = app2(a, x, y)
  }

  final implicit class Builder(val idx: Int) extends AnyVal {
    def v[A: Lam]: A = terms.vr(idx)
    def ![A: Lam](term: A): A = terms.app(v, term)
    def ![A: Lam](x: A, y: A): A = terms.app2(v, x, y)
  }


  def lam[A](term: A)(implicit lam: Lam[A]): A = lam.lam(term)
  def lam2[A: Lam](term: A): A = lam(lam(term))
  def lam3[A: Lam](term: A): A = lam(lam(lam(term)))
  def lam4[A: Lam](term: A): A = lam(lam(lam(lam(term))))
  def app[A](f: A, x: A)(implicit lam: Lam[A]): A = lam.app(f, x)
  def app2[A: Lam](f: A, x: A, y: A): A = app(app(f, x), y)
  def vr[A](name: Int)(implicit lam: Lam[A]): A = lam.v(name)

  def id[A: Lam]: A = lam(vr(1))
  def True[A: Lam]: A = lam(lam(vr(2)))
  def False[A: Lam]: A = lam(lam(vr(1)))

  def not[A: Lam]: A = lam(1 ! (False, True))
  def and[A: Lam]: A = lam2(1 ! (2.v, False))
  def or[A: Lam]: A = lam2(1 ! (True, 2.v))

  def zero[A: Lam]: A = lam2(1.v)
  def one[A: Lam]: A = lam2(2 ! 1.v)
  def two[A: Lam]: A = lam2(2 ! (2 ! 1.v))
  def three[A: Lam]: A = lam2(2 ! (2 ! (2 ! 1.v)))
  def four[A: Lam]: A = lam2(2 ! (2 ! (2 ! (2 ! 1.v))))
  def five[A: Lam]: A = lam2(2 ! (2 ! (2 ! (2 ! (2 ! 1.v)))))

  def succ[A: Lam]: A = lam3(2 ! (3 ! (2.v, 1.v)))
  def plus[A: Lam]: A = lam4(4 ! (2.v, 3 ! (2.v, 1.v)))
  def times[A: Lam]: A = lam2(2 ! (plus ! 1.v, zero))
  def pow[A: Lam]: A = lam2(1 ! (times ! 2.v, one))

  def fix[A: Lam]: A = lam(lam(2 ! (1 ! 1.v)) ! lam(2 ! (1 ! 1.v)))

  def pair[A: Lam]: A = lam3(1 ! (3.v, 2.v))
  def first[A: Lam]: A = lam(1 ! True)
  def second[A: Lam]: A = lam(1 ! False)
  def Ф[A: Lam]: A = lam2(1 ! (succ ! (first ! 2.v), first ! 2.v))
  def pred[A: Lam]: A = lam(second ! (1 ! (Ф, pair ! (zero, zero))))

  def minus[A: Lam]: A = lam2(1 ! (pred, 2.v))
  def is0[A: Lam] = lam(1 ! (lam(False), True))

  def le[A: Lam] = lam2(is0 ! (minus ! (2.v, 1.v)))
  def ge[A: Lam] = lam2(is0 ! (minus ! (1.v, 2.v)))
  def lt[A: Lam] = lam2(not ! (ge ! (2.v, 1.v)))
  def gt[A: Lam] = lam2(not ! (le ! (2.v, 1.v)))
  def eqn[A: Lam] = lam2(and ! (le ! (1.v, 2.v), ge ! (1.v, 2.v)))

  def fact[A: Lam] =
    fix ! lam2((is0 ! 1.v) ! (one, times ! (2 ! (pred ! 1.v), 1.v)))
}

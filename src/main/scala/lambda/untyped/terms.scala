package lambda.untyped

import lambda.untyped.Lam.Name

object terms {
  val X = Builder(0)
  val Y = Builder(1)
  val Z = Builder(2)
  val A = Builder(3)
  val B = Builder(4)
  val C = Builder(5)
  val D = Builder(6)


  final case class Builder(idx: Name) extends AnyVal {
    def v[A: Lam]: A = terms.v(idx)
    def lam[A: Lam](term: A): A = terms.lam(idx, term)
    def app[A: Lam](term: A): A = terms.app(v, term)
  }


  def lam[A](name: Name, term: A)(implicit lam: Lam[A]): A = lam.lam(name, term)
  def app[A](f: A, x: A)(implicit lam: Lam[A]): A = lam.app(f, x)
  def app2[A: Lam](f: A, x: A, y: A): A = app(app(f, x), y)
  def v[A](name: Name)(implicit lam: Lam[A]): A = lam.v(name)

  def id[A: Lam]: A = X.lam(X.v)
  def False[A: Lam]: A = X.lam(Y.lam(X.v))
  def True[A: Lam]: A = X.lam(Y.lam(Y.v))

  def zero[A: Lam]: A = X.lam(Y.lam(Y.v))
  def one[A: Lam]: A = X.lam(Y.lam(X.app(Y.v)))
  def two[A: Lam]: A = X.lam(Y.lam(X.app(X.app(Y.v))))
  def three[A: Lam]: A = X.lam(Y.lam(X.app(X.app(X.app(Y.v)))))
  def four[A: Lam]: A = X.lam(Y.lam(X.app(X.app(X.app(X.app(Y.v))))))
  def five[A: Lam]: A = X.lam(Y.lam(X.app(X.app(X.app(X.app(X.app(Y.v)))))))

  def succ[A: Lam](n: A): A = X.lam(Y.lam(X.app(app2(n, X.v, Y.v))))
  def plus[A: Lam](a: A, b: A): A = X.lam(Y.lam(app2(a, X.v, app2(b, X.v, Y.v))))
  def add[A: Lam]: A = A.lam(B.lam(plus(A.v, B.v)))
  def times[A: Lam](a: A, b: A): A = app2(a, app(add, b), zero)
  def mul[A: Lam]: A = A.lam(B.lam(times(A.v, B.v)))
  def exp[A: Lam](a: A, b: A): A = app2(b, app(mul, a), one)
  def pow[A: Lam]: A = A.lam(B.lam(exp(A.v, B.v)))
}
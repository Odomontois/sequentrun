package lambda.untyped

import lambda.untyped.terms._

object LApp extends App {
  def four[A: Lam] = app2[A](times, two, two)

  four[String]

  def steps(x: Term): Stream[String] =
    x.stepBN match {
      case Some(x1) => x.to[String] #:: steps(x1)
      case None => Stream(x.to[String])
    }


  def printsteps(x: Term) = steps(x).foreach(println)
  def printred(x: Term) = println(x.reduceBN.to[String])


  printred(app2(pow[Term], two[Term], four[Term]))
  printred(app2(eqn[Term], five[Term], five[Term]))
  printred(app(fact[Term], four[Term]))
}

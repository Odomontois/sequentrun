package lambda.untyped

import lambda.untyped.terms._

object LApp extends App {
  def four[A: Lam] = app2[A](times, two, two)

  four[String]

  def steps(x: Term): Stream[String] =
    x.stepBN match {
      case Some(x1) => x.to[String] #:: steps(x1)
      case None     => Stream(x.to[String])
    }

  def printsteps(x: Term) = steps(x).foreach(println)
  def printred(x: Term) = println(x.reduceBN.to[String])

  def `2^4`[A: Lam] = pow ! (two, four)
  def `5==5`[A: Lam] = eqn ! (five, five)
  def `4!`[A: Lam] = fact ! four

  printred(`2^4`[Term])
  printred(`5==5`[Term])
  printred(`4!`[Term])
}

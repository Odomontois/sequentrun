package lambda.untyped

import lambda.untyped.terms._

object LApp extends App {
  def four[A: Lam] = app2[A](mul, two, two)

  four[String]

  def steps(x: Term): Stream[String] =
    if (x.normal) Stream(x.to[String])
    else x.to[String] #:: steps(x.smallStepBN)

  println(four[Term].normal)

  steps(app2(mul[Term], two[Term], two[Term])).foreach(println)
  println(app2(pow[Term], two[Term], four[Term]).reduceBN.to[String])
}

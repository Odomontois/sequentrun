package lambda.untyped.simple

import terms._

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

  def `2^4`[A: Lam] = pow ! (two, four)
  def `5==5`[A: Lam] = eqn ! (five, five)
  def `4!`[A: Lam] = fact ! four
  def `8!`[A: Lam] = fact ! (plus ! (five, three))
  def `10=9`[A: Lam] = eqn ! (plus ! (five, five), plus ! (four, five))
  def `10=10`[A: Lam] = eqn ! (plus ! (five, five), plus ! (three, plus ! (four, three)))

  printred(`2^4`[Term])
  printred(`5==5`[Term])
  printred(`4!`[Term])

  println(Ev.runInt(`8!`[Ev.Run[Int]]))
  println(Ev.runBool(`10=9`[Ev.Run[Boolean]]))
  println(Ev.runBool(`10=10`[Ev.Run[Boolean]]))
}

package lambda.untyped.debrujin

import terms._
import scott._

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
  def `8!`[A: Lam] = fact ! (plus ! (five, three))
  def `10=9`[A: Lam] = eqn ! (plus ! (five, five), plus ! (four, five))
  def `10=10`[A: Lam] =
    eqn ! (plus ! (five, five), plus ! (three, plus ! (four, three)))

//  printsteps(succ[Term] ! two[Term])
//  printred(succ[Term])

//  printred(`2^4`[Term])
//  printred(`5==5`[Term])
//  printred(`10=9`[Term])
//  printred(`10=10`[Term])
//  printred(`4!`[Term])
//
//  println(Ev.runInt(`8!`[Ev.Run[Int]]))
//  println(Ev.runBool(`10=9`[Ev.Run[Boolean]]))
//  println(Ev.runBool(`10=10`[Ev.Run[Boolean]]))

  printred(s2[Term])
  printred(ssucc[Term] ! s1[Term])
  printred(s1[Term])
  printred(spred[Term] ! s2[Term])
  printred(s4[Term])
  printred(sadd[Term] ! (s2[Term], s2[Term]))
  printred(sadd2[Term] ! (s2[Term], s2[Term]))

  printred(smul[Term] ! (s2[Term], s2[Term]))

  printred(smul[Term] ! (s3[Term], s2[Term]))
  printred(smul[Term] ! (s2[Term], s3[Term]))

  printred(sminus[Term] ! (s4[Term], s2[Term]))

  printred(seq[Term] ! (s2[Term], s3[Term]))
  printred(seq[Term] ! (s3[Term], s2[Term]))
  printred(seq[Term] ! (s3[Term], s3[Term]))
}

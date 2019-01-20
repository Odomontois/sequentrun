package lambda.untyped.debrujin

import terms._

object scott {
  def s0[A: Lam] = lam2(2.v)
  def s1[A: Lam] = lam2(1.v ! s0)
  def s2[A: Lam] = lam2(1.v ! s1)
  def s3[A: Lam] = lam2(1.v ! s2)
  def s4[A: Lam] = lam2(1.v ! s3)

  def ssucc[A: Lam] = lam3(1 ! 3.v)
  def spred[A: Lam] = lam(1 ! (s0, lam(1.v)))

  def sadd[A: Lam] = fix ! lam3(2 ! (1.v, lam(ssucc ! (4 ! (1.v, 2.v)))))
}

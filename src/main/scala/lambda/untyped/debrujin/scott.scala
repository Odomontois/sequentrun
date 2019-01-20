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
  def sadd2[A: Lam] = fix ! lam2(1 ! (id, lam2(ssucc ! (4 ! (2.v, 1.v)))))

  def smul[A: Lam] = fix ! lam3(2 ! (s0, lam(sadd ! (2.v, 4 ! (1.v, 2.v)))))
  def sminus[A: Lam] = fix ! lam3(1 ! (2.v, lam(spred ! (4 ! (3.v, 1.v)))))

  def sle[A: Lam] = fix ! lam3(2 ! (True, lam(2 ! (False, lam(5 ! (2.v, 1.v))))))
  def sge[A: Lam] = fix ! lam3(1 ! (True, lam(3 ! (False, lam(5 ! (1.v, 2.v))))))
  def sgt[A: Lam] = lam2(not ! (sle ! (2.v ,1.v)))
  def slt[A: Lam] = lam2(not ! (sge ! (2.v ,1.v)))
  def seq[A: Lam] = fix ! lam3(2 ! (1 ! (True, lam(False)), lam(2 ! (False, lam(5 ! (2.v, 1.v))))))
}

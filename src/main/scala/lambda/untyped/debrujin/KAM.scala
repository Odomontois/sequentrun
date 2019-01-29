package lambda.untyped.debrujin
import lambda.untyped.debrujin.Term.La
import lib.iter
import terms._
import scott._

object KAM {
  sealed trait Clos {
    def step: Option[Clos] = step0(1)
    def step0(shift: Int): Option[Clos]
  }
  final case class Tm(term: Term, env: Vector[Clos]) extends Clos {
    override def step0(shift: Int): Option[Clos] = term match {
      case Term.Vr(v)    => env.lift(v - shift)
      case _             => None
      case Term.Ap(f, x) => Some(App(Tm(f, env), Tm(x, env)))
      //      case Term.La(t)    => Some(Lamb(Tm(t, env)))
    }
  }
  final case class App(f: Clos, x: Clos) extends Clos {
    override def step0(shift: Int): Option[Clos] = x match {
      case Tm(La(t), e) => Some(Tm(t, x +: e))
      case _            => f.step0(shift).map(App(_, x))
      //      case Lamb(t)      =>
    }
  }

//  final case class Lamb(t: Clos) extends Clos {
//    def step0(shift: Int): Option[Clos] = t.step0(shift + 1).map(Lamb)
//  }

  def run(term: Term): iter[Clos] = iter[Clos](Tm(term, Vector()))(_.step)
}

object KAMRun {
  def main(args: Array[String]): Unit = {
    //    println(KAM.run(succ[Term] ! one[Term]).run)
    KAM.run(succ[Term] ! one[Term]).steps.foreach(println)
  }
}

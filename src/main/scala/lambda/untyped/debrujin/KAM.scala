package lambda.untyped.debrujin

object KAM {
  sealed trait Clos {
    def step: Option[Clos]
  }
  final case class Tm(term: Term, env: Vector[Clos]) extends Clos {
    override def step: Option[Clos] = term match {
      case Term.Vr(v)    => env.lift(v)
      case Term.Ap(f, x) => Some(App(Tm(f, env), Tm(x, env)))
      case _             => None
    }
  }
  final case class App(f: Clos, x: Clos) extends Clos {
    override def step: Option[Clos] = ???
  }

}

package lambda.stlc
import lambda.stlc.Env.ElimEq
import util.=~=

trait Env[E] {
  def elimEq[F[_]](elim: ElimEq[F, E]): F[E]
}

object Env{
  type Empty = Empty.type
  case object Empty
  case class Cons[H: Type, T: Env](head: H, tail: T)

  trait ElimEq[F[_], E]{
    def empty(eq: E =~= Empty): F[Empty]
    def cons[H: Type, T: Env](eq: E =~= Cons[H, T]): F[Cons[H, T]]
  }
}

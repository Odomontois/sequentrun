package lib

trait Disjoint { type Tag }

trait DisjointAbsurd[T] extends Disjoint {
  type Tag = T
  def absurd(x: HasTag[Tag], y: HasNotTag[Tag]): Nothing = y.notTag(x.tag)
}

trait HasTag[T] extends DisjointAbsurd[T] {
  def tag: T
  def hasTag: HasTag[Tag] = this
}

trait HasNotTag[T] extends DisjointAbsurd[T] {
  def notTag: T => Nothing
  def hasNotTag: HasNotTag[Tag] = this
}

trait LeftVariant extends HasNotTag[Nothing] {
  def notTag: Nothing => Nothing = identity
}

trait RightVariant extends HasTag[Unit] {
  def tag = ()
}

object Disjoint {
  def absurd(x: LeftVariant with RightVariant): Nothing = x.absurd(x.hasTag, x.hasNotTag)
}

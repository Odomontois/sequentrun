package util

trait Disjoint { type Tag }

trait Lem[T] extends Disjoint {
  type Tag = T
  def nonContradiction: (T => Nothing) => T => Nothing = identity

  def absurd(x: HasTag[Tag], y: HasNotTag[Tag]): Nothing = x.nonContradiction(y.notTag)(x.tag)
}

trait HasTag[T] extends Lem[T] {
  def tag: T
  def hasTag: HasTag[Tag] = this
}

trait HasNotTag[T] extends Lem[T] {
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

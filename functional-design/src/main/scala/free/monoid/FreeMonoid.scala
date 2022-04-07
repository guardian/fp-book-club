package free.monoid

import cats.Monoid

import scala.annotation.tailrec

sealed abstract class FreeMonoid[A] {

  // this comes as part of Free monad foldMap but we have to implement our own.
  def foldApply[M: Monoid](function: A => M): M = {
    val monoid = summon[Monoid[M]]
    @tailrec
    def foldApply0(free: FreeMonoid[A], accu: M): M =
      free match {
        case Element(value) =>
          monoid.combine(accu, function(value))
        case Combined(Element(left), right) =>
          foldApply0(right, monoid.combine(accu, function(left)))
        case Combined(leftCombined: Combined[A], right) =>
          // rotate the tree around the left element, so we can start with the left most Element
          foldApply0(Combined(leftCombined.left, Combined(leftCombined.right, right)), accu)
      }
    foldApply0(this, monoid.empty)
  }

}

object FreeMonoid {

}

// notice that you can't have an empty free monoid or monad.  so it's more like a semigroup.
case class Element[A](value: A) extends FreeMonoid[A]
case class Combined[A](left: FreeMonoid[A], right: FreeMonoid[A]) extends FreeMonoid[A]

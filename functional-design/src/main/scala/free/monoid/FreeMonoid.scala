package free.monoid

import cats.Monoid

sealed abstract class FreeMonoid[A] {

  // this comes as part of Free monad but we have to implement our own.
  // this one is not tailrec
  def foldMap[M: Monoid](function: A => M): M = {
    val monoid = summon[Monoid[M]]
    this match {
      case Suspend(value) => function(value)
      case Combined(h, t) => monoid.combine(t.foldMap(function), h.foldMap(function))
    }
  }

  def combineWith(a: FreeMonoid[A]): FreeMonoid[A] =
    Combined(a, this)

}

object FreeMonoid {

}

// notice that you can't have an empty free monoid or monad.  so it's more like a semigroup.
case class Suspend[A](value: A) extends FreeMonoid[A]
case class Combined[A](h: FreeMonoid[A], t: FreeMonoid[A]) extends FreeMonoid[A]

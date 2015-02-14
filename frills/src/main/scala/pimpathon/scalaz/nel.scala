package pimpathon.scalaz

import scala.collection.{mutable => M, GenTraversable, GenTraversableLike}
import scalaz.NonEmptyList

import pimpathon.{CanBuildNonEmpty, genTraversableLike}

import pimpathon.list._
import pimpathon.tuple._


object nel extends genTraversableLike[NonEmptyList] {
  implicit def canBuildNonEmpty[A]: CanBuildNonEmpty[A, NonEmptyList[A]] = new CanBuildNonEmpty[A, NonEmptyList[A]] {
    def builder(head: A): M.Builder[A, NonEmptyList[A]] = List.newBuilder[A].mapResult(NonEmptyList.nel(head, _))
  }

  implicit class NelOps[A](val nel: NonEmptyList[A]) extends AnyVal {
    def distinct: NonEmptyList[A] = lift(_.distinct)
    def distinctBy[B](f: A => B): NonEmptyList[A] = lift(_.distinctBy(f))

    private def lift(f: List[A] => List[A]): NonEmptyList[A] = f(nel.list).headTail.calc(NonEmptyList.nel)
  }

  protected def toGTL[A](nel: NonEmptyList[A]): GenTraversableLike[A, GenTraversable[A]] = nel.list
}
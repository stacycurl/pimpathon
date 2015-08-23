package pimpathon.scalaz

import scala.collection.{GenTraversable, GenTraversableLike, mutable => M}
import scalaz.{NonEmptyList, Order, \/}

import pimpathon.CanBuildNonEmpty
import pimpathon.frills.genTraversableLike.GenTraversableLikeOfDisjunctionFrillsMixin

import pimpathon.genTraversableLike._
import pimpathon.list._
import pimpathon.tuple._


object nel {
  implicit def canBuildNonEmpty[A]: CanBuildNonEmpty[A, NonEmptyList[A]] = new CanBuildNonEmpty[A, NonEmptyList[A]] {
    def builder(head: A): M.Builder[A, NonEmptyList[A]] = List.newBuilder[A].mapResult(NonEmptyList.nel(head, _))
  }

  implicit class NelFrills[A](val nel: NonEmptyList[A])
    extends GenTraversableLikePimpsMixin[A, NonEmptyList] {

    def distinct: NonEmptyList[A] = lift(_.distinct)
    def distinctBy[B](f: A ⇒ B): NonEmptyList[A] = lift(_.distinctBy(f))
    def max(implicit o: Order[A]): A = nel.list.max(o.toScalaOrdering)
    def min(implicit o: Order[A]): A = nel.list.min(o.toScalaOrdering)

    private def lift(f: List[A] ⇒ List[A]): NonEmptyList[A] = f(nel.list).headTail.calc(NonEmptyList.nel)

    protected def gtl: GTLGT[A] = nel.list
  }

  implicit class NelOfEithersFrills[L, R](nel: NonEmptyList[Either[L, R]])
    extends GenTraversableLikeOfEitherPimpsMixin[L, R, NonEmptyList] {

    protected def gtl: GTLGT[Either[L, R]] = nel.list
  }

  implicit class NelOfTuple2Frills[K, V](nel: NonEmptyList[(K, V)]) extends GenTraversableLikeOfTuple2Mixin[K, V] {
    protected def gtl: GTLGT[(K, V)] = nel.list
  }

  implicit class NelOfDisjunctinonsFrills[L, R](nel: NonEmptyList[L \/ R])
    extends GenTraversableLikeOfDisjunctionFrillsMixin[L, R] {

    protected def gtl: GenTraversableLike[L \/ R, GenTraversable[L \/ R]] = nel.list
  }
}
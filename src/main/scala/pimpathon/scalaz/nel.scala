package pimpathon.scalaz

import scala.collection.{GenTraversable, GenTraversableLike}
import scalaz.{NonEmptyList, Order, \/}

import pimpathon.CanBuildNonEmpty
import pimpathon.frills.genTraversableLike.{GenTraversableLikeFrillsMixin, GenTraversableLikeOfDisjunctionFrillsMixin}

import pimpathon.function._
import pimpathon.genTraversableLike._
import pimpathon.list._
import pimpathon.tuple._


object nel {
  implicit def canBuildNonEmpty[A]: CanBuildNonEmpty[A, NonEmptyList[A]] = 
    (head: A) ⇒ List.newBuilder[A].mapResult(tail ⇒ NonEmptyList.fromSeq[A](head, tail))

  implicit class NelFrills[A](private val self: NonEmptyList[A]) extends GenTraversableLikeFrillsMixin[A, NonEmptyList] {
    def unique: NonEmptyList[A] = lift(_.distinct)
    def uniqueBy[B](f: A ⇒ B): NonEmptyList[A] = lift(_.distinctBy(f))
    def filter(p: Predicate[A]): Option[NonEmptyList[A]] = liftO(_.filter(p))
    def filterNot(p: Predicate[A]): Option[NonEmptyList[A]] = liftO(_.filterNot(p))
    def max(implicit o: Order[A]): A = toList.max(o.toScalaOrdering)
    def min(implicit o: Order[A]): A = toList.min(o.toScalaOrdering)
    def toList: List[A] = self.stream.toList

    private def lift(f: List[A] ⇒ List[A]): NonEmptyList[A] = toNel(f(toList).headTail)
    private def liftO(f: List[A] ⇒ List[A]): Option[NonEmptyList[A]] = f(toList).headTailOption.map(toNel)

    protected def gtl: GTLGT[A] = toList
    protected def cc: NonEmptyList[A] = self
    private def toNel(ht: (A, List[A])): NonEmptyList[A] = ht.calc(NonEmptyList.fromSeq)
  }

  implicit class NelOfEithersFrills[L, R](
    private val self: NonEmptyList[Either[L, R]]
  ) extends GenTraversableLikeOfEitherPimpsMixin[L, R, NonEmptyList] {

    protected def gtl: GTLGT[Either[L, R]] = self.toList
  }

  implicit class NelOfTuple2Frills[K, V](
    private val self: NonEmptyList[(K, V)]
  ) extends GenTraversableLikeOfTuple2Mixin[K, V] {

    protected def gtl: GTLGT[(K, V)] = self.toList
  }

  implicit class NelOfDisjunctinonsFrills[L, R](
    private val self: NonEmptyList[L \/ R]
  ) extends GenTraversableLikeOfDisjunctionFrillsMixin[L, R] {

    protected def gtl: GenTraversableLike[L \/ R, GenTraversable[L \/ R]] = self.toList
  }
}
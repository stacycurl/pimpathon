package pimpathon.frills

import pimpathon.genTraversableLike.GTLGT

import scala.collection.{GenTraversable, GenTraversableLike}
import scalaz.{NonEmptyList, \/}

import pimpathon.frills.genTraversableLike.{GenTraversableLikeFrillsMixin, GenTraversableLikeOfDisjunctionFrillsMixin}

import pimpathon.boolean._


object vector {
  implicit class VectorFrills[A](vector: Vector[A]) extends GenTraversableLikeFrillsMixin[A, Vector] {
    def toNel: Option[NonEmptyList[A]] = vector.nonEmpty.option(NonEmptyList(vector.head, vector.tail: _*))

    protected def gtl: GTLGT[A] = vector
    protected def cc: Vector[A] = vector
  }

  implicit class VectorOfDisjunctionsFrills[L, R](vector: Vector[L \/ R])
    extends GenTraversableLikeOfDisjunctionFrillsMixin[L, R] {

    protected def gtl: GenTraversableLike[L \/ R, GenTraversable[L \/ R]] = vector
  }
}
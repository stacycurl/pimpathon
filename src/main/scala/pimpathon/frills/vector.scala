package pimpathon.frills

import pimpathon.genTraversableLike.GTLGT

import scala.collection.{GenTraversable, GenTraversableLike}
import scalaz.{NonEmptyList, \/}

import pimpathon.frills.genTraversableLike.{GenTraversableLikeFrillsMixin, GenTraversableLikeOfDisjunctionFrillsMixin}

import pimpathon.boolean._


object vector {
  implicit class VectorFrills[A](self: Vector[A]) extends GenTraversableLikeFrillsMixin[A, Vector] {
    def toNel: Option[NonEmptyList[A]] = self.nonEmpty.option(NonEmptyList(self.head, self.tail: _*))

    protected def gtl: GTLGT[A] = self
    protected def cc: Vector[A] = self
  }

  implicit class VectorOfDisjunctionsFrills[L, R](self: Vector[L \/ R]) extends GenTraversableLikeOfDisjunctionFrillsMixin[L, R] {
    protected def gtl: GenTraversableLike[L \/ R, GenTraversable[L \/ R]] = self
  }
}
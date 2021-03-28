package pimpathon.frills

import pimpathon.genTraversableLike.GTLGT

import scala.collection.{GenTraversable, GenTraversableLike}
import scalaz.\/

import pimpathon.frills.genTraversableLike.{GenTraversableLikeFrillsMixin, GenTraversableLikeOfDisjunctionFrillsMixin}


object set {
  implicit class SetFrills[A](private val self: Set[A]) extends GenTraversableLikeFrillsMixin[A, Set] {
    protected def gtl: GTLGT[A] = self
    protected def cc: Set[A] = self
  }

  implicit class SetOfDisjunctionsFrills[L, R](
    private val self: Set[L \/ R]
  ) extends GenTraversableLikeOfDisjunctionFrillsMixin[L, R] {

    protected def gtl: GenTraversableLike[L \/ R, GenTraversable[L \/ R]] = self
  }
}
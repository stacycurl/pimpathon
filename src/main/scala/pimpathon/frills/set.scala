package pimpathon.frills

import pimpathon.genTraversableLike.GTLGT

import scala.collection.{GenTraversable, GenTraversableLike}
import scalaz.\/

import pimpathon.frills.genTraversableLike.{GenTraversableLikeFrillsMixin, GenTraversableLikeOfDisjunctionFrillsMixin}


object set {
  implicit class SetFrills[A](value: Set[A]) extends GenTraversableLikeFrillsMixin[A, Set] {
    protected def gtl: GTLGT[A] = value
    protected def cc: Set[A] = value
  }

  implicit class SetOfDisjunctionsFrills[L, R](set: Set[L \/ R])
    extends GenTraversableLikeOfDisjunctionFrillsMixin[L, R] {

    protected def gtl: GenTraversableLike[L \/ R, GenTraversable[L \/ R]] = set
  }
}
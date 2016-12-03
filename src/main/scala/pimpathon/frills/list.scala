package pimpathon.frills

import pimpathon.genTraversableLike.GTLGT

import scala.collection.{GenTraversable, GenTraversableLike}
import scala.collection.immutable.List
import scalaz.{NonEmptyList, \/}

import pimpathon.frills.genTraversableLike.{GenTraversableLikeFrillsMixin, GenTraversableLikeOfDisjunctionFrillsMixin}

import pimpathon.list._


object list {
  implicit class ListFrills[A](self: List[A]) extends GenTraversableLikeFrillsMixin[A, List] {
    def toNel: Option[NonEmptyList[A]] = self.unconsC(None, head ⇒ tail ⇒ Some(NonEmptyList(head, tail: _*)))

    protected def gtl: GTLGT[A] = self
    protected def cc: List[A]   = self
  }

  implicit class ListOfDisjunctionsFrills[L, R](self: List[L \/ R]) extends GenTraversableLikeOfDisjunctionFrillsMixin[L, R] {
    protected def gtl: GenTraversableLike[L \/ R, GenTraversable[L \/ R]] = self
  }
}
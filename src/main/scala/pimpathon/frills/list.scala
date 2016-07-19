package pimpathon.frills

import pimpathon.genTraversableLike.GTLGT

import scala.collection.{GenTraversable, GenTraversableLike}
import scala.collection.immutable.List
import scalaz.{NonEmptyList, \/}

import pimpathon.frills.genTraversableLike.{GenTraversableLikeFrillsMixin, GenTraversableLikeOfDisjunctionFrillsMixin}

import pimpathon.list._


object list {
  implicit class ListFrills[A](value: List[A]) extends GenTraversableLikeFrillsMixin[A, List] {
    def toNel: Option[NonEmptyList[A]] = value.unconsC(None, head ⇒ tail ⇒ Some(NonEmptyList(head, tail: _*)))

    protected def gtl: GTLGT[A] = value
    protected def cc: List[A] = value
  }

  implicit class ListOfDisjunctionsFrills[L, R](list: List[L \/ R])
    extends GenTraversableLikeOfDisjunctionFrillsMixin[L, R] {

    protected def gtl: GenTraversableLike[L \/ R, GenTraversable[L \/ R]] = list
  }
}
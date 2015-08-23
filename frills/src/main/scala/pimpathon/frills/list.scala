package pimpathon.frills

import scala.collection.{GenTraversable, GenTraversableLike}
import scala.collection.immutable.List
import scalaz.{NonEmptyList, \/}

import pimpathon.frills.genTraversableLike.GenTraversableLikeOfDisjunctionFrillsMixin

import pimpathon.list._
import scalaz.syntax.std.either._


object list {
  implicit class ListFrills[A](value: List[A]) {
    def onlyDisjunction: List[A] \/ A = value.onlyEither.disjunction
    def toNel: Option[NonEmptyList[A]] = value.unconsC(None, head ⇒ tail ⇒ Some(NonEmptyList(head, tail: _*)))
  }

  implicit class ListOfDisjunctionsFrils[L, R](list: List[L \/ R])
    extends GenTraversableLikeOfDisjunctionFrillsMixin[L, R] {

    protected def gtl: GenTraversableLike[L \/ R, GenTraversable[L \/ R]] = list
  }
}
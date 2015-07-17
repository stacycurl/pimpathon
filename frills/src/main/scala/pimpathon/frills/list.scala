package pimpathon.frills

import pimpathon.list._

import scala.collection.immutable.List
import scalaz.syntax.std.either._
import scalaz.{NonEmptyList, \/}


object list extends genTraversableLike[List] {
  implicit class ListFrills[A](val value: List[A]) extends AnyVal {
    def onlyDisjunction: List[A] \/ A = value.onlyEither.disjunction
    def toNel: Option[NonEmptyList[A]] = value.unconsC(None, head ⇒ tail ⇒ Some(NonEmptyList(head, tail: _*)))
  }
}

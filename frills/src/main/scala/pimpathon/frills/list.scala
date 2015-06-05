package pimpathon.frills

import scalaz.{\/, NonEmptyList}

import pimpathon.list._
import scalaz.syntax.std.either._


object list {
  implicit class ListFrills[A](val value: List[A]) extends AnyVal {
    def onlyDisjunction: List[A] \/ A = value.onlyEither.disjunction
    def toNel: Option[NonEmptyList[A]] = value.unconsC(None, head ⇒ tail ⇒ Some(NonEmptyList(head, tail: _*)))
  }
}

package pimpathon.frills

import scalaz.NonEmptyList

import pimpathon.list._


object list {
  implicit class ListFrills[A](val value: List[A]) extends AnyVal {
    def toNel: Option[NonEmptyList[A]] = value.unconsC(None, head ⇒ tail ⇒ Some(NonEmptyList(head, tail: _*)))
  }
}

package pimpathon.frills

import scalaz.NonEmptyList

import pimpathon.list._


object list {
  implicit def listFrills[A](value: List[A]): ListFrills[A] = new ListFrills[A](value)

  class ListFrills[A](value: List[A]) {
    def toNel: Option[NonEmptyList[A]] = value.unconsC(None, head ⇒ tail ⇒ Some(NonEmptyList(head, tail: _*)))
  }
}

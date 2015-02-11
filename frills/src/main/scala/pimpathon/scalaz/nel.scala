package pimpathon.scalaz

import scalaz.NonEmptyList

import pimpathon.list._
import pimpathon.tuple._


object nel {
  implicit class NelOps[A](nel: NonEmptyList[A]) {
    def distinct: NonEmptyList[A] =
      nel.list.distinct.headTail.calcC(head => tail => NonEmptyList(head, tail: _*))
  }
}

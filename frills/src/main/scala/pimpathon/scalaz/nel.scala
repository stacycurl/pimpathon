package pimpathon.scalaz

import pimpathon.list._
import pimpathon.tuple._

import scalaz.NonEmptyList


object nel {
  implicit class NelOps[A](nel: NonEmptyList[A]) {
    def distinct: NonEmptyList[A] = lift(_.distinct)
    def distinctBy[B](f: A => B): NonEmptyList[A] = lift(_.distinctBy(f))

    private def lift(f: List[A] => List[A]): NonEmptyList[A] =
      f(nel.list).headTail.calcC(head => tail => NonEmptyList(head, tail: _*))
  }
}

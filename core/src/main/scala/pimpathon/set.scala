package pimpathon

import scala.annotation.tailrec

import pimpathon.any._
import pimpathon.list._


object set {
  implicit class SetOps[A](val set: Set[A]) extends AnyVal {
    def powerSet: Set[Set[A]] = {
      def recurse(list: List[A]): List[List[A]] =
        list.unconsC(List(Nil), head => tail => recurse(tail).calc(ps => ps ++ ps.map(head :: _)))

      recurse(set.toList).map(_.toSet).toSet
    }
  }
}

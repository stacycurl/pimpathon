package pimpathon

import scala.collection.{mutable => M}

import pimpathon.any._
import pimpathon.list._


object set extends genTraversableLike[Set] {
  implicit class SetOps[A](val set: Set[A]) extends AnyVal {
    def powerSet: Set[Set[A]] = {
      def recurse(list: List[A]): List[List[A]] =
        list.unconsC(List(Nil), head => tail => recurse(tail).calc(ps => ps ++ ps.map(head :: _)))

      recurse(set.toList).map(_.toSet).toSet
    }

    def toMutable: M.Set[A] = mutable
    def mutable: M.Set[A] = M.Set.empty[A] ++ set
  }
}

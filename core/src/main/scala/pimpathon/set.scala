package pimpathon

import scala.annotation.tailrec
import scala.collection.{mutable => M}

import pimpathon.any._
import pimpathon.list._


object set {
  implicit def setOps[A](set: Set[A]): SetOps[A] = new SetOps[A](set)

  class SetOps[A](val set: Set[A]) {
    def powerSet: Set[Set[A]] = {
      def recurse(list: List[A]): List[List[A]] =
        list.unconsC(List(Nil), head => tail => recurse(tail).calc(ps => ps ++ ps.map(head :: _)))

      recurse(set.toList).map(_.toSet).toSet
    }

    def mutable: M.Set[A] = M.Set.empty[A] ++ set
    def toMutable: M.Set[A] = mutable
  }
}

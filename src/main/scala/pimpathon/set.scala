package pimpathon

import pimpathon.genTraversableLike.{GenTraversableLikeOfTuple2Mixin, GenTraversableLikeOfEitherPimpsMixin, GTLGT}

import scala.collection.immutable.TreeSet
import scala.collection.{mutable ⇒ M, GenTraversable, GenTraversableLike}

import pimpathon.any._
import pimpathon.list._


object set  {
  implicit class SetPimps[A](self: Set[A]) extends genTraversableLike.GenTraversableLikePimpsMixin[A, Set] {
    def sorted(implicit ordering: Ordering[A]): TreeSet[A] = TreeSet() ++ self

    def notContains(elem: A): Boolean = !self.contains(elem)

    def powerSet: Set[Set[A]] = {
      def recurse(list: List[A]): List[List[A]] =
        list.unconsC(List(Nil), head ⇒ tail ⇒ recurse(tail) |> (ps ⇒ ps ++ ps.map(head :: _)))

      recurse(self.toList).map(_.toSet).toSet
    }

    def toMutable: M.Set[A] = mutable
    def mutable: M.Set[A] = M.Set.empty[A] ++ self

    protected def gtl: GenTraversableLike[A, GenTraversable[A]] = self
    protected def cc: Set[A] = self
  }

  implicit class SetOfEitherPimps[L, R](self: Set[_ <: Either[L, R]]) extends GenTraversableLikeOfEitherPimpsMixin[L, R, Set] {
    protected def gtl: GTLGT[Either[L, R]] = self
  }

  implicit class SetOfTuple2Pimps[K, V](self: Set[(K, V)]) extends GenTraversableLikeOfTuple2Mixin[K, V] {
    protected def gtl: GTLGT[(K, V)] = self
  }
}
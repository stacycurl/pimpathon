package pimpathon

import pimpathon.genTraversableLike.{GenTraversableLikeOfTuple2Mixin, GenTraversableLikeOfEitherPimpsMixin, GTLGT}

import scala.collection.immutable.TreeSet
import scala.collection.{mutable ⇒ M, GenTraversable, GenTraversableLike}

import pimpathon.any._
import pimpathon.list._


object set  {
  implicit class SetPimps[A](set: Set[A]) extends genTraversableLike.GenTraversableLikePimpsMixin[A, Set] {
    def sorted(implicit ordering: Ordering[A]): TreeSet[A] = TreeSet() ++ set

    def notContains(elem: A): Boolean = !set.contains(elem)

    def powerSet: Set[Set[A]] = {
      def recurse(list: List[A]): List[List[A]] =
        list.unconsC(List(Nil), head ⇒ tail ⇒ recurse(tail) |> (ps ⇒ ps ++ ps.map(head :: _)))

      recurse(set.toList).map(_.toSet).toSet
    }

    def toMutable: M.Set[A] = mutable
    def mutable: M.Set[A] = M.Set.empty[A] ++ set

    protected def gtl: GenTraversableLike[A, GenTraversable[A]] = set
    protected def cc: Set[A] = set
  }

  implicit class SetOfEitherPimps[L, R](set: Set[_ <: Either[L, R]])
    extends GenTraversableLikeOfEitherPimpsMixin[L, R, Set] {

    protected def gtl: GTLGT[Either[L, R]] = set
  }

  implicit class SetOfTuple2Pimps[K, V](set: Set[(K, V)]) extends GenTraversableLikeOfTuple2Mixin[K, V] {
    protected def gtl: GTLGT[(K, V)] = set
  }
}
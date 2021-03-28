package pimpathon

import pimpathon.genTraversableLike.{GenTraversableLikeOfTuple2Mixin, GenTraversableLikeOfEitherPimpsMixin, GTLGT}


object vector {
  implicit class VectorPimps[A](private val self: Vector[A]) extends genTraversableLike.GenTraversableLikePimpsMixin[A, Vector] {
    protected def gtl: GTLGT[A] = self
    protected def cc: Vector[A] = self
  }

  implicit class VectorOfEitherPimps[L, R](
    private val self: Vector[_ <: Either[L, R]]
  ) extends GenTraversableLikeOfEitherPimpsMixin[L, R, Vector] {

    protected def gtl: GTLGT[Either[L, R]] = self
  }

  implicit class VectorOfTuple2Pimps[K, V](private val self: Vector[(K, V)]) extends GenTraversableLikeOfTuple2Mixin[K, V] {
    protected def gtl: GTLGT[(K, V)] = self
  }
}
package pimpathon

import pimpathon.genTraversableLike.{GenTraversableLikeOfTuple2Mixin, GenTraversableLikeOfEitherPimpsMixin, GTLGT}


object vector {
  implicit class VectorPimps[A](vector: Vector[A]) extends genTraversableLike.GenTraversableLikePimpsMixin[A, Vector] {
    protected def gtl: GTLGT[A] = vector
    protected def cc: Vector[A] = vector
  }

  implicit class VectorOfEitherPimps[L, R](vector: Vector[_ <: Either[L, R]])
    extends GenTraversableLikeOfEitherPimpsMixin[L, R, Vector] {

    protected def gtl: GTLGT[Either[L, R]] = vector
  }

  implicit class VectorOfTuple2Pimps[K, V](vector: Vector[(K, V)]) extends GenTraversableLikeOfTuple2Mixin[K, V] {
    protected def gtl: GTLGT[(K, V)] = vector
  }
}
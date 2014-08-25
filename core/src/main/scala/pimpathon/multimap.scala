package pimpathon

import scala.collection.generic.CanBuildFrom


object multiMap {
  type MultiMap[F[_], K, V] = Map[K, F[V]]

  implicit def build[F[_], K, V](implicit fcbf: CanBuildFrom[Nothing, V, F[V]])
    : CanBuildFrom[Nothing, (K, V), MultiMap[F, K, V]] = MultiMap.build

  object MultiMap {
    def build[F[_], K, V](implicit fcbf: CanBuildFrom[Nothing, V, F[V]])
      : CanBuildFrom[Nothing, (K, V), MultiMap[F, K, V]] = new MultiMapCanBuildFrom[F, K, V]

    def empty[F[_], K, V]: MultiMap[F, K, V] = Map.empty[K, F[V]]
  }
}

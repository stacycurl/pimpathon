package pimpathon

import scala.collection.{breakOut, mutable => M}
import scala.collection.generic.CanBuildFrom

import pimpathon.map._


object multiMap {
  type MultiMap[F[_], K, V] = Map[K, F[V]]

  implicit def build[F[_], K, V](implicit fcbf: CanBuildFrom[Nothing, V, F[V]])
    : CanBuildFrom[Nothing, (K, V), MultiMap[F, K, V]] = MultiMap.build

  implicit def multiMapOps[F[_], K, V](multiMap: MultiMap[F, K, V]): MultiMapOps[F, K, V] =
    new MultiMapOps[F, K, V](multiMap)

  class MultiMapOps[F[_], K, V](val multiMap: MultiMap[F, K, V]) {
    // just an alias for mapValuesEagerly
    def select[W](f: F[V] => W): Map[K, W] = multiMap.mapValuesEagerly(f)

    def merge(other: MultiMap[F, K, V])(
      implicit cbf: CanBuildFrom[F[V], V, F[V]], fTraversableOnce: F[V] <:< TraversableOnce[V]
    ): MultiMap[F, K, V] = if (multiMap.isEmpty) other else other.foldLeft(multiMap) {
      case (acc, (key, otherValues)) => acc.append(key, otherValues)
    }

    def append(key: K, newValues: F[V])(
      implicit cbf: CanBuildFrom[F[V], V, F[V]], fTraversableOnce: F[V] <:< TraversableOnce[V]
    ): MultiMap[F, K, V] = multiMap + ((key, multiMap.get(key) match {
      case None         => newValues
      case Some(values) => (cbf() ++= values ++= newValues).result()
    }))
  }

  object MultiMap {
    def build[F[_], K, V](implicit fcbf: CanBuildFrom[Nothing, V, F[V]])
      : CanBuildFrom[Nothing, (K, V), MultiMap[F, K, V]] = new MultiMapCanBuildFrom[F, K, V]

    def empty[F[_], K, V]: MultiMap[F, K, V] = Map.empty[K, F[V]]
  }


  trait IgnoreFromCBF[-From, -Elem, +To] extends CanBuildFrom[From, Elem, To] {
    override def apply(from: From): M.Builder[Elem, To] = apply()
  }

  class MultiMapCanBuildFrom[F[_], K, V](implicit fcbf: CanBuildFrom[Nothing, V, F[V]])
    extends CanBuildFrom[Nothing, (K, V), MultiMap[F, K, V]]
    with IgnoreFromCBF[Nothing, (K, V), MultiMap[F, K, V]] {

    def apply(): M.Builder[(K, V), MultiMap[F, K, V]] = new MultiMapBuilder[F, K, V]
  }

  class MultiMapBuilder[F[_], K, V](
    map: M.Map[K, M.Builder[V, F[V]]] = M.Map.empty[K, M.Builder[V, F[V]]]
  )(
    implicit fcbf: CanBuildFrom[Nothing, V, F[V]]
  )
    extends M.Builder[(K, V), MultiMap[F, K, V]] {

    def +=(elem: (K, V)): this.type = add(elem._1, elem._2)
    def clear(): Unit = map.clear()
    def result(): Map[K, F[V]] = map.map(kv => (kv._1, kv._2.result()))(breakOut)

    def add(k: K, v: V): this.type = {
      map.put(k, map.getOrElse(k, fcbf.apply()) += v)

      this
    }
  }
}

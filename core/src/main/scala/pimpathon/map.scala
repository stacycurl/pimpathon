package pimpathon

import pimpathon.multiMap._


object map {
  implicit def mapOps[K, V](map: Map[K, V]): MapOps[K, V] = new MapOps[K, V](map)

  class MapOps[K, V](map: Map[K, V]) {
    def getOrThrow(k: K, message: String): V =
      getOrThrow(k, new IllegalArgumentException(message))

    def getOrThrow(k: K, exception: Exception): V =
      map.getOrElse(k, throw exception)

    def emptyTo(empty: => Map[K, V]): Map[K, V] = uncons(empty, _ => map)

    def mapNonEmpty[A](f: Map[K, V] => A): Option[A] = if (map.isEmpty) None else Some(f(map))
    def uncons[A](empty: => A, nonEmpty: Map[K, V] => A): A = if (map.isEmpty) empty else nonEmpty(map)

    def keyForMaxValue(implicit O: Ordering[V]): Option[K] = mapNonEmpty(_.maxBy(value)).map(key)
    def keyForMinValue(implicit O: Ordering[V]): Option[K] = mapNonEmpty(_.minBy(value)).map(key)
    def valueForMaxKey(implicit O: Ordering[K]): Option[V] = mapNonEmpty(_.maxBy(key)).map(value)
    def valueForMinKey(implicit O: Ordering[K]): Option[V] = mapNonEmpty(_.minBy(key)).map(value)

    def mapValuesEagerly[W](f: V => W): Map[K, W] = map.map { case (k, v) => (k, f(v)) }(collection.breakOut)

    @inline private def key:   ((K, V)) => K = (kv: (K, V)) => kv._1
    @inline private def value: ((K, V)) => V = (kv: (K, V)) => kv._2
  }
}

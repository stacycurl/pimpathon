package pimpathon

import scala.collection.GenTraversableOnce

import pimpathon.multiMap._


object map {
  implicit def mapOps[K, V](map: Map[K, V]): MapOps[K, V] = new MapOps[K, V](map)

  class MapOps[K, V](map: Map[K, V]) {
    def containsAny(ok: Option[K]): Boolean = ok.exists(map.contains)
    def containsAll(ok: Option[K]): Boolean = ok.forall(map.contains)
    def containsAny[GK <: GenTraversableOnce[K]](gk: GK): Boolean = gk.exists(map.contains)
    def containsAll[GK <: GenTraversableOnce[K]](gk: GK): Boolean = gk.forall(map.contains)

    def get(ok: Option[K]): Option[V] = ok.flatMap(map.get)

    def getOrThrow(k: K, message: String): V =
      getOrThrow(k, new IllegalArgumentException(message))

    def getOrThrow(k: K, exception: Exception): V =
      map.getOrElse(k, throw exception)

    def emptyTo(empty: => Map[K, V]): Map[K, V] = uncons(empty, _ => map)

    def mapNonEmpty[A](f: Map[K, V] => A): Option[A] = if (map.isEmpty) None else Some(f(map))
    def uncons[A](empty: => A, nonEmpty: Map[K, V] => A): A = if (map.isEmpty) empty else nonEmpty(map)

    def keyForMaxValue(implicit O: Ordering[V]): Option[K] = entryForMaxValue.map(key)
    def keyForMinValue(implicit O: Ordering[V]): Option[K] = entryForMinValue.map(key)
    def valueForMaxKey(implicit O: Ordering[K]): Option[V] = entryForMaxKey.map(value)
    def valueForMinKey(implicit O: Ordering[K]): Option[V] = entryForMinKey.map(value)

    def entryForMaxValue(implicit O: Ordering[V]): Option[(K, V)] = mapNonEmpty(_.maxBy(value))
    def entryForMinValue(implicit O: Ordering[V]): Option[(K, V)] = mapNonEmpty(_.minBy(value))
    def entryForMaxKey(implicit O: Ordering[K]): Option[(K, V)] = mapNonEmpty(_.maxBy(key))
    def entryForMinKey(implicit O: Ordering[K]): Option[(K, V)] = mapNonEmpty(_.minBy(key))

    def mapValuesEagerly[W](f: V => W): Map[K, W] = map.map { case (k, v) => (k, f(v)) }(collection.breakOut)

    @inline private def key:   ((K, V)) => K = (kv: (K, V)) => kv._1
    @inline private def value: ((K, V)) => V = (kv: (K, V)) => kv._2
  }
}

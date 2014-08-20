package pimpathon

import scala.collection.GenTraversableOnce
import scala.collection.{mutable => M}

import pimpathon.function._
import pimpathon.multiMap._


object map {
  implicit class MapOps[K, V](val map: Map[K, V]) extends AnyVal {
    def containsAny(ok: Option[K]): Boolean = ok.exists(map.contains)
    def containsAll(ok: Option[K]): Boolean = ok.forall(map.contains)
    def containsAny[GK <: GenTraversableOnce[K]](gk: GK): Boolean = gk.exists(map.contains)
    def containsAll[GK <: GenTraversableOnce[K]](gk: GK): Boolean = gk.forall(map.contains)

    def get(ok: Option[K]): Option[V]             = ok.flatMap(map.get)
    def getOrThrow(k: K, message: String): V      = getOrThrow(k, new IllegalArgumentException(message))
    def getOrThrow(k: K, exception: => Exception): V = map.getOrElse(k, throw exception)

    def findKey(p: Predicate[K]): Option[K]                 = findEntryWithKey(p).map(_._1)
    def findValue(p: Predicate[V]): Option[V]               = findEntryWithValue(p).map(_._2)
    def findEntryWithKey(p: Predicate[K]): Option[(K, V)]   = map.find(kv => p(kv._1))
    def findEntryWithValue(p: Predicate[V]): Option[(K, V)] = map.find(kv => p(kv._2))

    def filterKeysNot(p: Predicate[K]): Map[K, V]   = map.filterNot(kv => p(kv._1))
    def filterValuesNot(p: Predicate[V]): Map[K, V] = map.filterNot(kv => p(kv._2))
    def filterValues(p: Predicate[V]): Map[K, V]    = map.filter(kv => p(kv._2))

    def valueExists(p: Predicate[V]): Boolean = map.exists(kv => p(kv._2))

    def emptyTo(empty: => Map[K, V]): Map[K, V]             = uncons(empty, _ => map)
    def mapNonEmpty[A](f: Map[K, V] => A): Option[A]        = if (map.isEmpty) None else Some(f(map))
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

    def reverseToMultiMap: MultiMap[Set, V, K] = map.map(_.swap)(collection.breakOut)

    def mutable: M.Map[K, V] = M.Map.empty[K, V] ++ map

    @inline private def key:   ((K, V)) => K = (kv: (K, V)) => kv._1
    @inline private def value: ((K, V)) => V = (kv: (K, V)) => kv._2
  }

  implicit class MultiMapOps[F[_], K, V](val multiMap: MultiMap[F, K, V]) extends AnyVal {
    // just an alias for mapValuesEagerly
    def select[W](f: F[V] => W): Map[K, W] = multiMap.mapValuesEagerly(f)
  }
}

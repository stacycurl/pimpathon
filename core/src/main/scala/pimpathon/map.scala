package pimpathon

import scala.collection.{GenTraversableOnce, mutable => M}
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{SortedMap, TreeMap}

import pimpathon.function._
import pimpathon.multiMap._
import pimpathon.tuple._


object map {
  implicit def mapOps[K, V](map: Map[K, V]): MapOps[K, V] = new MapOps[K, V](map)

  class MapOps[K, V](map: Map[K, V]) {
    def containsAny(ok: Option[K]): Boolean = ok.exists(map.contains)
    def containsAll(ok: Option[K]): Boolean = ok.forall(map.contains)
    def containsAny[GK <: GenTraversableOnce[K]](gk: GK): Boolean = gk.exists(map.contains)
    def containsAll[GK <: GenTraversableOnce[K]](gk: GK): Boolean = gk.forall(map.contains)

    def get(ok: Option[K]): Option[V]                = ok.flatMap(map.get)
    def getOrThrow(k: K, message: String): V         = getOrThrow(k, new IllegalArgumentException(message))
    def getOrThrow(k: K, exception: => Exception): V = map.getOrElse(k, throw exception)

    def findKey(p: Predicate[K]): Option[K]   = keyFor.matchingKey(p)
    def findValue(p: Predicate[V]): Option[V] = valueFor.matchingValue(p)

    def filterKeysNot(p: Predicate[K]): Map[K, V]   = map.filterNot(kv => p(kv._1))
    def filterValuesNot(p: Predicate[V]): Map[K, V] = map.filterNot(kv => p(kv._2))
    def filterValues(p: Predicate[V]): Map[K, V]    = map.filter(kv => p(kv._2))

    def valueExists(p: Predicate[V]): Boolean = map.exists(kv => p(kv._2))

    def emptyTo(empty: => Map[K, V]): Map[K, V]             = uncons(empty, _ => map)
    def mapNonEmpty[A](f: Map[K, V] => A): Option[A]        = if (map.isEmpty) None else Some(f(map))
    def uncons[A](empty: => A, nonEmpty: Map[K, V] => A): A = if (map.isEmpty) empty else nonEmpty(map)

    def mapKeysEagerly[C](f: K => C): Map[C, V]   = map.map { case (k, v) => (f(k), v) }(collection.breakOut)
    def mapValuesEagerly[W](f: V => W): Map[K, W] = map.map { case (k, v) => (k, f(v)) }(collection.breakOut)

    def reverse(f: Set[K] => K): Map[V, K] = reverseToMultiMap.mapValuesEagerly(f)
    def reverseToMultiMap: MultiMap[Set, V, K] = map.map(_.swap)(collection.breakOut)

    def sorted(implicit ordering: Ordering[K]): SortedMap[K, V] = TreeMap.empty[K, V](ordering) ++ map

    def andThenM[W](other: Map[V, W]): Map[K, W] = map.flatMap(kv => other.get(kv._2).map(kv._1 -> _))

    def mutable: M.Map[K, V] = M.Map.empty[K, V] ++ map
    def toMutable: M.Map[K, V] = mutable

    def entryFor: MapAndThen[K, V, (K, V)] = new MapAndThen[K, V, (K, V)](map, identity[(K, V)])
    def keyFor:   MapAndThen[K, V, K]      = new MapAndThen[K, V, K](map, key)
    def valueFor: MapAndThen[K, V, V]      = new MapAndThen[K, V, V](map, value)

    def partitionKeysBy[C](pf: PartialFunction[K, C]): (Map[C, V], Map[K, V]) =
      map.partition(kv => pf.isDefinedAt(kv._1)).tmap(_.mapKeysEagerly(pf), identity)

    def partitionValuesBy[W](pf: PartialFunction[V, W]): (Map[K, W], Map[K, V]) =
      map.partition(kv => pf.isDefinedAt(kv._2)).tmap(_.mapValuesEagerly(pf), identity)
  }

  class MapAndThen[K, V, A](map: Map[K, V], andThen: ((K, V)) => A) {
    def maxValue(implicit O: Ordering[V]): Option[A] = map.mapNonEmpty(_.maxBy(value)).map(andThen)
    def minValue(implicit O: Ordering[V]): Option[A] = map.mapNonEmpty(_.minBy(value)).map(andThen)
    def maxKey(implicit O: Ordering[K]): Option[A] = map.mapNonEmpty(_.maxBy(key)).map(andThen)
    def minKey(implicit O: Ordering[K]): Option[A] = map.mapNonEmpty(_.minBy(key)).map(andThen)

    def matchingKey(p: Predicate[K]): Option[A]   = map.find(kv => p(kv._1)).map(andThen)
    def matchingValue(p: Predicate[V]): Option[A] = map.find(kv => p(kv._2)).map(andThen)
  }

  @inline private def key[K, V]:   ((K, V)) => K = (kv: (K, V)) => kv._1
  @inline private def value[K, V]: ((K, V)) => V = (kv: (K, V)) => kv._2
}

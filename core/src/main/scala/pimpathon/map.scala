package pimpathon

import scala.collection.{breakOut, mutable => M, GenTraversable, GenTraversableOnce}
import scala.collection.immutable.{SortedMap, TreeMap}

import pimpathon.function._
import pimpathon.multiMap._
import pimpathon.tuple._


object map extends genTraversableLike[GenTraversable] {
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


    def keyExists(p: Predicate[K]): Boolean = map.exists(kv => p(kv._1))
    def valueExists(p: Predicate[V]): Boolean = map.exists(kv => p(kv._2))

    def emptyTo(empty: => Map[K, V]): Map[K, V]             = uncons(empty, _ => map)
    def mapNonEmpty[A](f: Map[K, V] => A): Option[A]        = if (map.isEmpty) None else Some(f(map))
    def uncons[A](empty: => A, nonEmpty: Map[K, V] => A): A = if (map.isEmpty) empty else nonEmpty(map)

    def reverse(f: Set[K] => K): Map[V, K] = reverseToMultiMap.mapValuesEagerly(f)
    def reverseToMultiMap: MultiMap[Set, V, K] = map.map(_.swap)(breakOut)

    def sorted(implicit ordering: Ordering[K]): SortedMap[K, V] = TreeMap.empty[K, V](ordering) ++ map

    def composeM[C](other: Map[C, K]): Map[C, V] = other.andThenM(map)
    def andThenM[W](other: Map[V, W]): Map[K, W] = updateValues(other.get _)

    def toMutable: M.Map[K, V] = mutable
    def mutable: M.Map[K, V] = M.Map.empty[K, V] ++ map

    def entryFor: MapAndThen[K, V, (K, V)] = new MapAndThen[K, V, (K, V)](map, identity[(K, V)])
    def keyFor:   MapAndThen[K, V, K]      = new MapAndThen[K, V, K](map, key)
    def valueFor: MapAndThen[K, V, V]      = new MapAndThen[K, V, V](map, value)

    def partitionKeysBy[C](pf: PartialFunction[K, C]): (Map[K, V], Map[C, V])   = partitionEntriesBy(pf.first[V])
    def partitionValuesBy[W](pf: PartialFunction[V, W]): (Map[K, V], Map[K, W]) = partitionEntriesBy(pf.second[K])

    def partitionEntriesBy[C, W](pf: PartialFunction[(K, V), (C, W)]): (Map[K, V], Map[C, W]) =
      map.partition(pf.isUndefinedAt).tmap(identity, _.map(pf))

    def mapKeysEagerly[C](f: K => C): Map[C, V]          = map.map { case (k, v) => (f(k), v) }(breakOut)
    def mapValuesEagerly[W](f: V => W): Map[K, W]        = map.map { case (k, v) => (k, f(v)) }(breakOut)
    def mapEntries[C, W](f: K => V => (C, W)): Map[C, W] = map.map { case (k, v) => f(k)(v)   }(breakOut)

    def updateValue(key: K, f: V => Option[V]): Map[K, V] =
      map.get(key).flatMap(f).map(newValue => map + ((key, newValue))).getOrElse(map - key)

    def updateKeys[C](pf: PartialFunction[K, C]): Map[C, V] = updateKeys(pf.lift)
    def updateValues[W](pf: PartialFunction[V, W]): Map[K, W] = updateValues(pf.lift)

    def updateKeys[C](f: K => Option[C]): Map[C, V]   = map.flatMap(kv => f(kv._1).map(_ -> kv._2))
    def updateValues[W](f: V => Option[W]): Map[K, W] = map.flatMap(kv => f(kv._2).map(kv._1 -> _))
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

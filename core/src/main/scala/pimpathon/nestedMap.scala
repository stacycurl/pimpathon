package pimpathon

import pimpathon.multiMap.IgnoreFromCBF

import scala.collection.generic.CanBuildFrom
import scala.collection.{mutable => M, breakOut}

import pimpathon.map._


object nestedMap {
  type NestedMap[K1, K2, V] = Map[K1, Map[K2, V]]
  type NMCBF[K1, K2, V] = CanBuildFrom[Nothing, (K1, K2, V), NestedMap[K1, K2, V]]

  implicit def build[K1, K2, V]: NMCBF[K1, K2, V] = new NestedMapCanBuilderFrom[K1, K2, V]

  implicit class NestedMapOps[K1, K2, V](val value: NestedMap[K1, K2, V]) extends AnyVal {
    def flipNesting: NestedMap[K2, K1, V] = value.flatMap(o => o._2.map(i => (i._1, o._1, i._2)))(breakOut)
    def nestedMap: NestedMapConflictingOps[K1, K2, V] = new NestedMapConflictingOps[K1, K2, V](value)
  }

  class NestedMapConflictingOps[K1, K2, V](value: NestedMap[K1, K2, V]) {
    def mapValuesEagerly[W](f: V => W): NestedMap[K1, K2, W] = value.mapValuesEagerly(_.mapValuesEagerly(f))
    def mapKeysEagerly[C](f: K2 => C): NestedMap[K1, C, V]   = value.mapValuesEagerly(_.mapKeysEagerly(f))
  }

  object NestedMap {
    def build[K1, K2, V]: NMCBF[K1, K2, V] = new NestedMapCanBuilderFrom[K1, K2, V]
    def empty[K1, K2, V]: NestedMap[K1, K2, V] = Map.empty[K1, Map[K2, V]]
  }

  class NestedMapCanBuilderFrom[K1, K2, V] extends NMCBF[K1, K2, V]
    with IgnoreFromCBF[Nothing, (K1, K2, V), NestedMap[K1, K2, V]] {

    def apply(): M.Builder[(K1, K2, V), NestedMap[K1, K2, V]] = new NestedMapBuilder[K1, K2, V]()
  }

  class NestedMapBuilder[K1, K2, V](map: M.Map[K1, Map[K2, V]] = M.Map.empty[K1, Map[K2, V]])
    extends M.Builder[(K1, K2, V), NestedMap[K1, K2, V]] {

    def +=(elem: (K1, K2, V)): this.type = { add(elem._1, elem._2, elem._3); this}
    def result(): NestedMap[K1, K2, V] = map.map(entry => entry)(breakOut)
    def clear(): Unit = map.clear()

    private def add(k1: K1, k2: K2, v: V): Unit = map.put(k1, map.getOrElse(k1, Map.empty[K2, V]) + ((k2, v)))
  }
}

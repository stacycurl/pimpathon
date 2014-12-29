package pimpathon

import pimpathon.map._


object nestedMap {
  type NestedMap[K1, K2, V] = Map[K1, Map[K2, V]]

  implicit class NestedMapOps[K1, K2, V](val value: NestedMap[K1, K2, V]) extends AnyVal {
    def nestedMap: NestedMapConflictingOps[K1, K2, V] = new NestedMapConflictingOps[K1, K2, V](value)
  }

  class NestedMapConflictingOps[K1, K2, V](value: NestedMap[K1, K2, V]) {
    def mapValuesEagerly[W](f: V => W): NestedMap[K1, K2, W] = value.mapValuesEagerly(_.mapValuesEagerly(f))
  }
}

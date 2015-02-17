package pimpathon

import scala.collection.{mutable ⇒ M}

import pimpathon.function._


object mutableMap {
  implicit def mutableMapOps[K, V](map: M.Map[K, V]): MutableMapOps[K, V] = new MutableMapOps[K, V](map)

  class MutableMapOps[K, V](map: M.Map[K, V]) {
    def retainKeys(p: Predicate[K]): M.Map[K, V]   = map.retain((k, _) ⇒ p(k))
    def retainValues(p: Predicate[V]): M.Map[K, V] = map.retain((_, v) ⇒ p(v))
  }
}
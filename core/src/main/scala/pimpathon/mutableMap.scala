package pimpathon

import scala.collection.{mutable ⇒ M}

import pimpathon.function._


object mutableMap {
  implicit def mutableMapPimps[K, V](map: M.Map[K, V]): MutableMapPimps[K, V] = new MutableMapPimps[K, V](map)

  class MutableMapPimps[K, V](map: M.Map[K, V]) {
    def retainKeys(p: Predicate[K]): M.Map[K, V]   = map.retain((k, _) ⇒ p(k))
    def retainValues(p: Predicate[V]): M.Map[K, V] = map.retain((_, v) ⇒ p(v))
  }
}
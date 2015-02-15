package pimpathon

import scala.collection.{mutable => M}

import pimpathon.function._


object mutableMap {
  implicit class MutableMapOps[K, V](val map: M.Map[K, V]) extends AnyVal {
    def retainKeys(p: Predicate[K]): M.Map[K, V] = map.retain((k, _) => p(k))
  }
}
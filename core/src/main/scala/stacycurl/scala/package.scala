package stacycurl.scala

import scala.collection.generic.CanBuildFrom
import scalaz._



package object pimpathon {
  type MultiMap[K, V] = Map[K, List[V]]

  implicit def build[K, V]: CanBuildFrom[Any, (K, V), MultiMap[K, V]] = MultiMap.build

  object MultiMap {
    def build[K, V]: CanBuildFrom[Any, (K, V), MultiMap[K, V]] = new MultiMapCanBuildFrom[Any, K, V]
  }
}

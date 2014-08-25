package pimpathon

import scala.collection.breakOut
import scala.collection.generic.{CanBuildFrom, FilterMonadic}

import pimpathon.multiMap._


object filterMonadic extends filterMonadic

trait filterMonadic {
  implicit def filterMonadicTuple2Ops[K, V, Repr](fm: FilterMonadic[(K, V), Repr])
    : FilterMonadicTuple2Ops[K, V, Repr] = new FilterMonadicTuple2Ops[K, V, Repr](fm)

  class FilterMonadicTuple2Ops[K, V, Repr](fm: FilterMonadic[(K, V), Repr]) {
    def toMultiMap[F[_]](implicit fcbf: CanBuildFrom[Nothing, V, F[V]])
      : MultiMap[F, K, V] = fm.map(kv => kv)(breakOut)
  }
}

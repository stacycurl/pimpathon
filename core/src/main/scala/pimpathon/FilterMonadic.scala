package pimpathon

import scala.collection.breakOut
import scala.collection.generic.FilterMonadic

import pimpathon.multiMap._


object filterMonadic extends filterMonadic

trait filterMonadic {
  implicit def filterMonadicTuple2Pimps[K, V, Repr](fm: FilterMonadic[(K, V), Repr])
    : FilterMonadicTuple2Pimps[K, V, Repr] = new FilterMonadicTuple2Pimps[K, V, Repr](fm)

  class FilterMonadicTuple2Pimps[K, V, Repr](fm: FilterMonadic[(K, V), Repr]) {
    def toMultiMap[F[_]](implicit fcbf: CCBF[V, F]): MultiMap[F, K, V] = fm.map(kv ⇒ kv)(breakOut)
  }
}
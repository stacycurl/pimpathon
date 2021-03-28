package pimpathon

import pimpathon.multiMap.IgnoreFromCBF

import scala.collection.generic.CanBuildFrom
import scala.collection.{breakOut, mutable => M}
import scala.collection.immutable.{Map => ▶:}
import pimpathon.builder.BuilderPimps
import pimpathon.map.MapPimps


object nestedMap {
  type NMCBF[K1, K2, V] = CanBuildFrom[Nothing, (K1, K2, V), K1 ▶: K2 ▶: V]

  implicit def build[K1, K2, V]: NMCBF[K1, K2, V] = new NestedMapCanBuilderFrom[K1, K2, V]

  implicit class NestedMapPimps[K1, K2, V](val self: K1 ▶: K2 ▶: V) extends AnyVal {
    def flipNesting:                  K2 ▶: K1 ▶: V = self.flatMap(o ⇒ o._2.map(i ⇒ (i._1, o._1, i._2)))(breakOut)
    def +(kkv: (K1, K2, V)):          K1 ▶: K2 ▶: V = append(kkv._1, kkv._2, kkv._3)
    def append(k1: K1, k2: K2, v: V): K1 ▶: K2 ▶: V = self + ((k1, self.getOrEmpty(k1) + ((k2, v))))

    def getOrEmpty(k1: K1): K2 ▶: V = self.getOrElse(k1, Map.empty[K2, V])

    def nestedMap: NestedMapConflictingPimps[K1, K2, V] = new NestedMapConflictingPimps[K1, K2, V](self)
  }

  class NestedMapConflictingPimps[K1, K2, V](private val self: K1 ▶: K2 ▶: V) {
    def mapValuesEagerly[W](f: V ⇒ W): K1 ▶: K2 ▶: W = self.mapValuesEagerly(_.mapValuesEagerly(f))
    def mapKeysEagerly[C](f: K2 ⇒ C):  K1 ▶: C ▶: V  = self.mapValuesEagerly(_.mapKeysEagerly(f))

    def mapEntries[C1, C2, W](f: (K1, K2, V) ⇒ (C1, C2, W)): C1 ▶: C2 ▶: W =
      build[C1, C2, W]().run(_ ++= (for { (k1, k2v) <- self; (k2, v) <- k2v } yield f(k1, k2, v)))
  }

  object NestedMap {
    def build[K1, K2, V]: NMCBF[K1, K2, V] = new NestedMapCanBuilderFrom[K1, K2, V]
    def empty[K1, K2, V]: K1 ▶: K2 ▶: V = Map.empty[K1, K2 ▶: V]
  }

  class NestedMapCanBuilderFrom[K1, K2, V] extends NMCBF[K1, K2, V]
    with IgnoreFromCBF[Nothing, (K1, K2, V), K1 ▶: K2 ▶: V] {

    def apply(): M.Builder[(K1, K2, V), K1 ▶: K2 ▶: V] = new NestedMapBuilder[K1, K2, V]()
  }

  class NestedMapBuilder[K1, K2, V](map: M.Map[K1, K2 ▶: V] = M.Map.empty[K1, K2 ▶: V])
    extends M.Builder[(K1, K2, V), K1 ▶: K2 ▶: V] {

    def +=(elem: (K1, K2, V)): this.type = { add(elem._1, elem._2, elem._3); this}
    def result(): K1 ▶: K2 ▶: V = map.map(entry ⇒ entry)(breakOut)
    def clear(): Unit = map.clear()

    private def add(k1: K1, k2: K2, v: V): Unit = map.put(k1, map.getOrElse(k1, Map.empty[K2, V]) + ((k2, v)))
  }
}
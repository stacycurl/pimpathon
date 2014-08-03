package pimpathon

import scalaz.Order
import scalaz.std.iterable._
import scalaz.syntax.foldable._


object map {
  implicit class MapOps[K, V](val map: Map[K, V]) extends AnyVal {
    def getOrThrow(k: K, message: => String): V = map.getOrElse(k, throw new RuntimeException(message))

    def emptyTo(empty: => Map[K, V]): Map[K, V] = uncons(empty, _ => map)

    def uncons[A](empty: => A, nonEmpty: Map[K, V] => A): A = if (map.isEmpty) empty else nonEmpty(map)

    def keyForMaxValue(implicit O: Ordering[V]): Option[K] = map.maximum(value).map(_._1)
    def keyForMinValue(implicit O: Ordering[V]): Option[K] = map.minimum(value).map(_._1)
    def valueForMaxKey(implicit O: Ordering[K]): Option[V] = map.maximum(key).map(_._2)
    def valueForMinKey(implicit O: Ordering[K]): Option[V] = map.minimum(key).map(_._2)

    def mapValuesEagerly[W](f: V => W): Map[K, W] = map.map { case (k, v) => (k, f(v)) }(collection.breakOut)

    private def key(implicit O: Ordering[K]): Order[(K, V)] =
      Order.fromScalaOrdering(O).contramap[(K, V)](_._1)

    private def value(implicit O: Ordering[V]): Order[(K, V)] =
      Order.fromScalaOrdering(O).contramap[(K, V)](_._2)
  }
}

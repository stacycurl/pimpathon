package stacycurl.scala.pimpathon

import scalaz.Order
import scalaz.std.iterable._
import scalaz.syntax.foldable._


object map {
  implicit class MapOps[K, V](val map: Map[K, V]) extends AnyVal {
    def getOrThrow(k: K, message: => String): V = map.getOrElse(k, throw new RuntimeException(message))

    def emptyTo(empty: => Map[K, V]): Map[K, V] = uncons(empty, _ => map)

    def uncons[A](empty: => A, nonEmpty: Map[K, V] => A): A = if (map.isEmpty) empty else nonEmpty(map)

    def keyForMaxValue(implicit O: Ordering[V]): Option[K] =
      map.maximum(Order.fromScalaOrdering(O).contramap[(K, V)](_._2)).map(_._1)

    def valueForMaxKey(implicit O: Ordering[K]): Option[V] =
      map.maximum(Order.fromScalaOrdering(O).contramap[(K, V)](_._1)).map(_._2)

    def valueForMinKey(implicit O: Ordering[K]): Option[V] =
      map.minimum(Order.fromScalaOrdering(O).contramap[(K, V)](_._1)).map(_._2)
  }
}

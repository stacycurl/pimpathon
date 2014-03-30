package stacycurl.scala.pimpathon


object map {
  implicit class MapOps[K, V](map: Map[K, V]) {
    def getOrThrow(k: K, message: => String): V = map.getOrElse(k, throw new RuntimeException(message))

    def uncons[A](empty: => A, nonEmpty: Map[K, V] => A): A = if (map.isEmpty) empty else nonEmpty(map)
  }
}

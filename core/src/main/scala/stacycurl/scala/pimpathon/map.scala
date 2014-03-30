package stacycurl.scala.pimpathon


object map {
  implicit class MapOps[K, V](map: Map[K, V]) {
    def getOrThrow(k: K, message: => String): V = map.getOrElse(k, throw new RuntimeException(message))
  }
}

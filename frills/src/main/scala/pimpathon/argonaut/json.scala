package pimpathon.argonaut

import argonaut._
import pimpathon.function.Predicate

import argonaut.Json._
import scalaz.std.iterable._


object json {
  implicit class JsonOps(json: Json) {
    def filterNulls: Json = filter(_ != Json.jNull)

    private[argonaut] def filter(p: Predicate[Json]): Json =
      if (!p(json)) Json.jNull else json.withObject(filter(p, _)).withArray(filter(p, _))

    private def filter(p: Predicate[Json], obj: JsonObject): JsonObject =
      JsonObject.from(obj.toMap.collect { case (k, v) if p(v) => (k, v.filter(p)) })

    private def filter(p: Predicate[Json], array: JsonArray): JsonArray =
      array.collect { case j if p(j) => j.filter(p) }
  }
}

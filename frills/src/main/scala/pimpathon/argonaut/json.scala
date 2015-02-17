package pimpathon.argonaut

import argonaut._
import argonaut.Json._
import pimpathon.function.Predicate


object json {
  implicit def jsonOps(json: Json): JsonOps = new JsonOps(json)

  class JsonOps(json: Json) {
    def filterNulls: Json = filter(_ != Json.jNull)

    private[argonaut] def filter(p: Predicate[Json]): Json =
      if (!p(json)) Json.jNull else json.withObject(filter(p, _)).withArray(filter(p, _))

    private def filter(p: Predicate[Json], obj: JsonObject): JsonObject =
      JsonObject(obj.toInsertionMap.filter(p).map(_.filter(p)))

    private def filter(p: Predicate[Json], array: JsonArray): JsonArray =
      array.collect { case j if p(j) ⇒ j.filter(p) }
  }
}
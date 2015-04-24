package pimpathon.argonaut

import argonaut._
import argonaut.Json._
import pimpathon.function._


object json {
  implicit def jsonFrills(json: Json): JsonFrills = new JsonFrills(json)

  class JsonFrills(json: Json) {
    def filterNulls: Json = filterR(_ != jNull)

    private[argonaut] def filterR(p: Predicate[Json]): Json =
      p.cond(json.withObject(_.filterR(p)).withArray(_.filterR(p)), jNull)(json)
  }

  private implicit def jsonObjectFrills(o: JsonObject): JsonObjectFrills = new JsonObjectFrills(o)

  private class JsonObjectFrills(o: JsonObject) {
    private[argonaut] def filterR(p: Predicate[Json]): JsonObject =
      JsonObject(o.toInsertionMap.filter(p).map(_.filterR(p)))
  }

  private implicit def jsonArrayFrills(a: JsonArray): JsonArrayFrills = new JsonArrayFrills(a)

  private class JsonArrayFrills(a: JsonArray) {
    private[argonaut] def filterR(p: Predicate[Json]): JsonArray =
      a.collect { case j if p(j) ⇒ j.filterR(p) }
  }
}
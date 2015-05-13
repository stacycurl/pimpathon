package pimpathon.argonaut

import argonaut._
import argonaut.Json._
import pimpathon.function._


object json {
  implicit def jsonFrills(json: Json): JsonFrills = new JsonFrills(json)
  implicit def codecJsonFrills[A](codec: CodecJson[A]): CodecJsonFrills[A] = new CodecJsonFrills[A](codec)
  implicit def decodeJsonFrills[A](decoder: DecodeJson[A]): DecodeJsonFrills[A] = new DecodeJsonFrills[A](decoder)
  implicit def encodeJsonFrills[A](encoder: EncodeJson[A]): EncodeJsonFrills[A] = new EncodeJsonFrills[A](encoder)

  class JsonFrills(json: Json) {
    def filterNulls: Json = filterR(_ != jNull)

    private[argonaut] def filterR(p: Predicate[Json]): Json =
      p.cond(json.withObject(_.filterR(p)).withArray(_.filterR(p)), jNull)(json)
  }

  class CodecJsonFrills[A](value: CodecJson[A]) {
    def andThen(f: Json ⇒ Json): CodecJson[A] = CodecJson.derived[A](value.Encoder andThen f, value.Decoder)
    def compose(f: Json ⇒ Json): CodecJson[A] = CodecJson.derived[A](value.Encoder, value.Decoder compose f)
  }

  class DecodeJsonFrills[A](value: DecodeJson[A]) {
    def compose(f: Json ⇒ Json): DecodeJson[A] = DecodeJson[A](hc ⇒ value.decode(hc >-> f))
  }

  class EncodeJsonFrills[A](value: EncodeJson[A]) {
    def andThen(f: Json ⇒ Json): EncodeJson[A] = EncodeJson[A](a ⇒ f(value.encode(a)))
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
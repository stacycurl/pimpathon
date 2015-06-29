package pimpathon.argonaut

import argonaut._
import pimpathon.function.Predicate

import argonaut.Json._
import pimpathon.function._
import pimpathon.map._
import scalaz.std.iterable._


object json {
  implicit class JsonFrills(val value: Json) extends AnyVal {
    def filterNulls: Json = filterR(_ != jNull)

    private[argonaut] def filterR(p: Predicate[Json]): Json =
      p.cond(value.withObject(_.filterR(p)).withArray(_.filterR(p)), jNull)(value)
  }

  implicit class CodecJsonFrills[A](val value: CodecJson[A]) extends AnyVal {
    def andThen(f: Json ⇒ Json): CodecJson[A] = CodecJson.derived[A](value.Encoder andThen f, value.Decoder)
    def compose(f: Json ⇒ Json): CodecJson[A] = CodecJson.derived[A](value.Encoder, value.Decoder compose f)
  }

  implicit class DecodeJsonFrills[A](val value: DecodeJson[A]) extends AnyVal {
    def compose(f: Json ⇒ Json): DecodeJson[A] = DecodeJson[A](hc ⇒ value.decode(hc >-> f))
    def upcast[B >: A]: DecodeJson[B] = value.map[B](a ⇒ a: B)
  }

  implicit class EncodeJsonFrills[A](val value: EncodeJson[A]) extends AnyVal {
    def andThen(f: Json ⇒ Json): EncodeJson[A] = EncodeJson[A](a ⇒ f(value.encode(a)))
    def downcast[B <: A]: EncodeJson[B] = value.contramap[B](b ⇒ b: A)
  }

  private implicit class JsonObjectFrills(val o: JsonObject) extends AnyVal {
    private[argonaut] def filterR(p: Predicate[Json]): JsonObject =
      JsonObject.from(o.toMap.collectValues { case j if p(j) ⇒ j.filterR(p) })
  }

  private implicit class JsonArrayFrills(val a: JsonArray) extends AnyVal {
    private[argonaut] def filterR(p: Predicate[Json]): JsonArray =
      a.collect { case j if p(j) ⇒ j.filterR(p) }
  }
}
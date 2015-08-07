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

  implicit class CodecJsonMapFrills[K, V](val value: CodecJson[Map[K, V]]) extends AnyVal {
    def xmapKeys[C](kc: K ⇒ C)(ck: C ⇒ K): CodecJson[Map[C, V]] =
      CodecJson.derived[Map[C, V]](value.Encoder contramapKeys ck, value.Decoder mapKeys kc)
  }

  implicit class DecodeJsonFrills[A](val value: DecodeJson[A]) extends AnyVal {
    def compose(f: Json ⇒ Json): DecodeJson[A] = DecodeJson[A](hc ⇒ value.decode(hc >-> f))
    def upcast[B >: A]: DecodeJson[B] = value.map[B](a ⇒ a: B)
  }

  implicit class DecodeJsonMapFrills[K, V](val value: DecodeJson[Map[K, V]]) extends AnyVal {
    def mapKeys[C](f: K ⇒ C): DecodeJson[Map[C, V]] = value.map(_.mapKeysEagerly(f))
    def mapValues[W](f: V ⇒ W): DecodeJson[Map[K, W]] = value.map(_.mapValuesEagerly(f))
  }

  implicit class EncodeJsonFrills[A](val value: EncodeJson[A]) extends AnyVal {
    def andThen(f: Json ⇒ Json): EncodeJson[A] = EncodeJson[A](a ⇒ f(value.encode(a)))
    def downcast[B <: A]: EncodeJson[B] = value.contramap[B](b ⇒ b: A)
  }

  implicit class EncodeJsonMapFrills[K, V](val value: EncodeJson[Map[K, V]]) extends AnyVal {
    def contramapKeys[C](f: C ⇒ K): EncodeJson[Map[C, V]] = value.contramap[Map[C, V]](_.mapKeysEagerly(f))
    def contramapValues[W](f: W ⇒ V): EncodeJson[Map[K, W]] = value.contramap[Map[K, W]](_.mapValuesEagerly(f))
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
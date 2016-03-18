package pimpathon.argonaut

import scala.language.higherKinds

import argonaut._
import monocle.{Prism, Traversal}
import pimpathon.function.Predicate

import argonaut.Json._
import pimpathon.any._
import pimpathon.function._
import pimpathon.map._
import scalaz.\/
import scalaz.std.iterable._


object json {
  implicit class JsonFrills(val value: Json) extends AnyVal {
    def filterNulls: Json = filterR(_ != jNull)

    private[argonaut] def filterR(p: Predicate[Json]): Json =
      p.cond(value.withObject(_.filterR(p)).withArray(_.filterR(p)), jNull)(value)
  }

  implicit class CodecJsonFrills[A](val value: CodecJson[A]) extends AnyVal {
    def beforeDecode(f: Json ⇒ Json): CodecJson[A] = compose(f)
    def afterDecode(f: A ⇒ A):  CodecJson[A] = derived(encoder ⇒ encoder)(_ map f)
    def beforeEncode(f: A ⇒ A): CodecJson[A] = derived(_ contramap f)(decoder ⇒ decoder)
    def afterEncode(f: Json ⇒ Json): CodecJson[A] = andThen(f)
    def andThen(f: Json ⇒ Json): CodecJson[A] = derived(_ andThen f)(decoder ⇒ decoder)
    def compose(f: Json ⇒ Json): CodecJson[A] = derived(encoder ⇒ encoder)(_ compose f)
    def xmapDisjunction[B](f: A => String \/ B)(g: B => A): CodecJson[B] = derived(_ beforeEncode g)(_ afterDecode f)

    private[argonaut] def derived[B](f: EncodeJson[A] ⇒ EncodeJson[B])(g: DecodeJson[A] ⇒ DecodeJson[B]) =
      CodecJson.derived[B](f(value.Encoder), g(value.Decoder))
  }

  implicit class CodecJsonMapFrills[K, V](val value: CodecJson[Map[K, V]]) extends AnyVal {
    def xmapKeys[C](kc: K ⇒ C)(ck: C ⇒ K): CodecJson[Map[C, V]]   = value.derived(_ contramapKeys ck)(_ mapKeys kc)
    def xmapValues[W](vw: V ⇒ W)(wv: W ⇒ V): CodecJson[Map[K, W]] = value.derived(_ contramapValues wv)(_ mapValues vw)
  }

  implicit class DecodeJsonFrills[A](val value: DecodeJson[A]) extends AnyVal {
    def beforeDecode(f: Json ⇒ Json): DecodeJson[A] = compose(f)
    def compose(f: Json ⇒ Json): DecodeJson[A] = DecodeJson[A](hc ⇒ value.decode(hc >-> f))
    def upcast[B >: A]: DecodeJson[B] = value.map[B](a ⇒ a: B)

    private[argonaut] def afterDecode[B](f: A => String \/ B): DecodeJson[B] = // Probably publish later
      DecodeJson[B](c => value.decode(c).flatMap(a => DecodeResult[B](f(a).leftMap(_ → c.history))))
  }

  implicit class DecodeJsonMapFrills[K, V](val value: DecodeJson[Map[K, V]]) extends AnyVal {
    def mapKeys[C](f: K ⇒ C): DecodeJson[Map[C, V]] = value.map(_.mapKeysEagerly(f))
    def mapValues[W](f: V ⇒ W): DecodeJson[Map[K, W]] = value.map(_.mapValuesEagerly(f))
  }

  implicit class EncodeJsonFrills[A](val value: EncodeJson[A]) extends AnyVal {
    def afterEncode(f: Json ⇒ Json): EncodeJson[A] = andThen(f)
    def andThen(f: Json ⇒ Json): EncodeJson[A] = EncodeJson[A](a ⇒ f(value.encode(a)))
    def downcast[B <: A]: EncodeJson[B] = value.contramap[B](b ⇒ b: A)

    private[argonaut] def beforeEncode[B](f: B => A): EncodeJson[B] = value contramap f // Probably publish later
  }

  implicit class EncodeJsonMapFrills[K, V](val value: EncodeJson[Map[K, V]]) extends AnyVal {
    def contramapKeys[C](f: C ⇒ K): EncodeJson[Map[C, V]] = value.contramap[Map[C, V]](_.mapKeysEagerly(f))
    def contramapValues[W](f: W ⇒ V): EncodeJson[Map[K, W]] = value.contramap[Map[K, W]](_.mapValuesEagerly(f))
  }

  implicit class TraversalFrills[A, B](val traversal: Traversal[A, B]) {
    def bool[That](  implicit cpf: CanPrismFrom[B, Boolean, That]): Traversal[A, That] = cpf(traversal)
    def string[That](implicit cpf: CanPrismFrom[B, String,  That]): Traversal[A, That] = cpf(traversal)
    def int[That](   implicit cpf: CanPrismFrom[B, Int,     That]): Traversal[A, That] = cpf(traversal)
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

case class CanPrismFrom[From, Elem, To](prism: Prism[From, To]) {
  def apply[A](traversal: Traversal[A, From]): Traversal[A, To] = traversal composePrism prism
}

object CanPrismFrom {
  implicit val cpfJsonToBoolean: CanPrismFrom[Json, Boolean, Boolean] = apply(jBoolPrism)
  implicit val cpfJsonToString:  CanPrismFrom[Json, String,  String]  = apply(jStringPrism)
  implicit val cpfJsonToInt:     CanPrismFrom[Json, Int,     Int   ]  = apply(jIntPrism)
}
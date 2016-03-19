package pimpathon.argonaut

import scala.language.higherKinds

import argonaut._
import monocle.{Prism, Traversal}
import monocle.{function ⇒ F, Iso}

import scala.util.Try

import monocle.std.list.listFilterIndex
import monocle.syntax.ApplyTraversal
import pimpathon.function.Predicate

import argonaut.Json._
import pimpathon.any._
import pimpathon.argonaut.json._
import pimpathon.function._
import pimpathon.map._
import pimpathon.string._
import scalaz.\/
import scalaz.std.iterable._


object json {
  implicit class JsonFrills(val value: Json) extends AnyVal {
    def descendant(path: String): ApplyTraversal[Json, Json, Json, Json] =
      ApplyTraversal(value, Traversal.id[Json].descendant(path))

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

  implicit class TraversalFrills[A, B](val traversal: Traversal[A, B]) extends AnyVal {
    def bool[That](  implicit cpf: CanPrismFrom[B, Boolean,    That]): Traversal[A, That] = cpf(traversal)
    def number[That](implicit cpf: CanPrismFrom[B, JsonNumber, That]): Traversal[A, That] = cpf(traversal)
    def string[That](implicit cpf: CanPrismFrom[B, String,     That]): Traversal[A, That] = cpf(traversal)
    def array[That]( implicit cpf: CanPrismFrom[B, List[Json], That]): Traversal[A, That] = cpf(traversal)
    def obj[That](   implicit cpf: CanPrismFrom[B, JsonObject, That]): Traversal[A, That] = cpf(traversal)

    def double[That](    implicit cpf: CanPrismFrom[B, Double,     That]): Traversal[A, That] = cpf(traversal)
    def int[That](       implicit cpf: CanPrismFrom[B, Int,        That]): Traversal[A, That] = cpf(traversal)
    def float[That](     implicit cpf: CanPrismFrom[B, Float,      That]): Traversal[A, That] = cpf(traversal)
    def short[That](     implicit cpf: CanPrismFrom[B, Short,      That]): Traversal[A, That] = cpf(traversal)
    def byte[That](      implicit cpf: CanPrismFrom[B, Byte,       That]): Traversal[A, That] = cpf(traversal)
    def bigDecimal[That](implicit cpf: CanPrismFrom[B, BigDecimal, That]): Traversal[A, That] = cpf(traversal)
    def bigInt[That](    implicit cpf: CanPrismFrom[B, BigInt,     That]): Traversal[A, That] = cpf(traversal)
  }

  private implicit class JsonObjectFrills(val o: JsonObject) extends AnyVal {
    private[argonaut] def filterR(p: Predicate[Json]): JsonObject =
      JsonObject.from(o.toMap.collectValues { case j if p(j) ⇒ j.filterR(p) })
  }

  implicit class TraversalToJsonFrills[A](val traversal: Traversal[A, Json]) extends AnyVal {
    def descendant(path: String): Traversal[A, Json] = path.split("/").filter(_.nonEmpty).foldLeft(traversal) {
      case (acc, "*")                                ⇒ (acc composeIso arrayObjectIso).obj composeTraversal F.each
      case (acc, subPath) if subPath.startsWith("[") ⇒ acc.array composeTraversal F.filterIndex(indecies(subPath))
      case (acc, subPath)                            ⇒ acc.obj composeTraversal F.filterIndex(keys(subPath))
    }
  }

  private implicit class JsonArrayFrills(val a: JsonArray) extends AnyVal {
    private[argonaut] def filterR(p: Predicate[Json]): JsonArray =
      a.collect { case j if p(j) ⇒ j.filterR(p) }
  }

  implicit class PrismFrills[A, B](val prism: Prism[A, B]) {
    def toList: Prism[List[A], List[B]] =
      Prism[List[A], List[B]](la ⇒ la.flatMap(prism.getOption).ifSelf(_.size == la.size))(_.map(prism.reverseGet))

    def toMap[K]: Prism[Map[K, A], Map[K, B]] = Prism[Map[K, A], Map[K, B]](mapKA ⇒ {
      mapKA.updateValues(a ⇒ prism.getOption(a)).ifSelf(_.size == mapKA.size)
    })((mapKB: Map[K, B]) ⇒ {
      mapKB.mapValuesEagerly(prism.reverseGet)
    })
  }

  type SApplyTraversal[From, To] = ApplyTraversal[From, From, To, To]

  implicit class ApplyTraversalFrills[From, To](val at: SApplyTraversal[From, To]) extends AnyVal {
    def bool[That](  implicit cpf: CanPrismFrom[To, Boolean,    That]): SApplyTraversal[From, That] = cpf(at)
    def number[That](implicit cpf: CanPrismFrom[To, JsonNumber, That]): SApplyTraversal[From, That] = cpf(at)
    def string[That](implicit cpf: CanPrismFrom[To, String,     That]): SApplyTraversal[From, That] = cpf(at)
    def array[That]( implicit cpf: CanPrismFrom[To, List[Json], That]): SApplyTraversal[From, That] = cpf(at)
    def obj[That](   implicit cpf: CanPrismFrom[To, JsonObject, That]): SApplyTraversal[From, That] = cpf(at)

    def double[That](    implicit cpf: CanPrismFrom[To, Double,     That]): SApplyTraversal[From, That] = cpf(at)
    def int[That](       implicit cpf: CanPrismFrom[To, Int,        That]): SApplyTraversal[From, That] = cpf(at)
    def float[That](     implicit cpf: CanPrismFrom[To, Float,      That]): SApplyTraversal[From, That] = cpf(at)
    def short[That](     implicit cpf: CanPrismFrom[To, Short,      That]): SApplyTraversal[From, That] = cpf(at)
    def byte[That](      implicit cpf: CanPrismFrom[To, Byte,       That]): SApplyTraversal[From, That] = cpf(at)
    def bigDecimal[That](implicit cpf: CanPrismFrom[To, BigDecimal, That]): SApplyTraversal[From, That] = cpf(at)
    def bigInt[That](    implicit cpf: CanPrismFrom[To, BigInt,     That]): SApplyTraversal[From, That] = cpf(at)
  }

  private val arrayObjectIso: Iso[Json, Json] = Iso[Json, Json](
    j ⇒ j.array.filter(_.nonEmpty).fold(j)(array ⇒ jObjectFields(ints zip array: _*)))(
    j ⇒ j.obj.filter(_.isNotEmpty).flatMap(o ⇒ Try(jArray(o.toMap.sortBy(_.toInt).values.toList)).toOption).getOrElse(j)
  )

  private def keys(value: String): Set[String]  = value.stripAffixes("{", "}").split(",").map(_.trim).toSet
  private def indecies(value: String): Set[Int] = value.stripAffixes("[", "]").split(",").map(_.trim.toInt).toSet
  private def ints: Stream[String]              = Stream.iterate(0)(_ + 1).map(_.toString)
}


case class CanPrismFrom[From, Elem, To](prism: Prism[From, To]) {
  def apply[A](applyTraversal: ApplyTraversal[A, A, From, From]): ApplyTraversal[A, A, To, To] = applyTraversal composePrism prism
  def apply[A](traversal: Traversal[A, From]): Traversal[A, To] = traversal composePrism prism
  def toList: CanPrismFrom[List[From], Elem, List[To]] = CanPrismFrom(prism.toList)
  def toMap[K]: CanPrismFrom[Map[K, From], Elem, Map[K, To]] = CanPrismFrom(prism.toMap[K])
}

object CanPrismFrom {
  implicit val cpfJsonToBoolean:    CanPrismFrom[Json, Boolean,    Boolean]    = apply(jBoolPrism)
  implicit val cpfJsonToJsonNumber: CanPrismFrom[Json, JsonNumber, JsonNumber] = apply(jNumberPrism)
  implicit val cpfJsonToString:     CanPrismFrom[Json, String,     String]     = apply(jStringPrism)
  implicit val cpfJsonToJsonArray:  CanPrismFrom[Json, List[Json], List[Json]] = apply(jArrayPrism)
  implicit val cpfJsonToJsonObject: CanPrismFrom[Json, JsonObject, JsonObject] = apply(jObjectPrism)
  implicit val cpfJsonToBigDecimal: CanPrismFrom[Json, BigDecimal, BigDecimal] = apply(jBigDecimalPrism)
  implicit val cpfJsonToDouble:     CanPrismFrom[Json, Double,     Double]     = apply(jDoublePrism)
  implicit val cpfJsonToFloat:      CanPrismFrom[Json, Float,      Float]      = apply(jFloatPrism)
  implicit val cpfJsonToBigInt:     CanPrismFrom[Json, BigInt,     BigInt]     = apply(jBigIntPrism)
  implicit val cpfJsonToLong:       CanPrismFrom[Json, Long,       Long]       = apply(jLongPrism)
  implicit val cpfJsonToInt:        CanPrismFrom[Json, Int,        Int]        = apply(jIntPrism)
  implicit val cpfJsonToShort:      CanPrismFrom[Json, Short,      Short]      = apply(jShortPrism)
  implicit val cpfJsonToByte:       CanPrismFrom[Json, Byte,       Byte]       = apply(jBytePrism)

  implicit def cpfl[From, Elem, To](implicit cpf: CanPrismFrom[From, Elem, To])
    : CanPrismFrom[List[From], Elem, List[To]] = cpf.toList

  implicit def cpfm[From, Elem, To](implicit cpf: CanPrismFrom[From, Elem, To])
    : CanPrismFrom[Map[String, From], Elem, Map[String, To]] = cpf.toMap

  implicit def cpfJsonObjectToTypedMap[/*From, Elem, */Value](
    implicit cpfv: CanPrismFrom[Json, Value, Value]
  ): CanPrismFrom[JsonObject, Value, Map[String, Value]] = {
    apply(jsonObjectMapIso.composePrism(cpfv.toMap[String].prism))
  }

  private val jsonObjectMapIso: Iso[JsonObject, Map[String, Json]] =
    Iso[JsonObject, Map[String, Json]](_.toMap)(map ⇒ JsonObject.from[Seq](map.toSeq))
}

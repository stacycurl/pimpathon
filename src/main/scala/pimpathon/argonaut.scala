package pimpathon

import scala.language.{dynamics, higherKinds, implicitConversions}

import _root_.argonaut.Json.{jFalse, jNull, jString, jTrue}
import _root_.argonaut.JsonObjectMonocle.{jObjectEach, jObjectFilterIndex}
import _root_.argonaut.{CodecJson, DecodeJson, DecodeResult, EncodeJson, HCursor, Json, JsonMonocle, JsonNumber, JsonObject, JsonParser, Parse, PrettyParams}
import _root_.java.io.File
import _root_.scalaz.{Applicative, \/}
import io.gatling.jsonpath.AST._
import io.gatling.jsonpath._
import monocle.function.Each.each
import monocle.function.FilterIndex.filterIndex
import monocle.std.list.{listEach, listFilterIndex}
import monocle.{Iso, Optional, Prism, Traversal}
import pimpathon.any.AnyPimps
import pimpathon.boolean.BooleanPimps
import pimpathon.file.FilePimps
import pimpathon.function.{Predicate, PredicatePimps}
import pimpathon.list.ListOfTuple2Pimps
import pimpathon.map.MapPimps
import pimpathon.string.StringPimps

import scala.{PartialFunction => ~>}
import scala.collection.immutable.{ListMap, Map => ▶:}
import scala.util.matching.Regex
import pimpathon.either._
import pimpathon.list._


object argonaut {
  private[pimpathon] implicit class RegexMatcher(val self: StringContext) extends AnyVal {
    def r: Regex = self.parts.mkString("(.+)").r
  }
  
  implicit class JsonCompanionFrils(val self: Json.type) extends AnyVal {
    def fromProperties(properties: Map[String, String]): Either[(String, String), Json] = {
      properties.apoFold[Json, (String, String)](Json.jEmptyObject) {
        case (acc, (key, value)) => for {
          json <- JsonParser.parse(value).leftMap(key -> _)
        } yield acc.append(key.split("\\.").toList.emptyTo(List(key)), json)
      }
    }
    
    def readFrom(file: File): Option[Json] = for {
      content <- if (file.exists()) Some(file.readString) else None
      json    <- Parse.parse(content) match {
        case Left(_)     ⇒ None
        case Right(json) ⇒ Some(json)
      }
    } yield json
  }

  implicit class JsonFrills(val self: Json) extends AnyVal {
    def descendant: Descendant[Json, Json, Json] =
      Descendant(self, List(Traversal.id[Json]), () ⇒ List("" -> Traversal.id[Json]))

    def descendant(paths: String*): Descendant[Json, Json, Json] = Descendant(self,
      paths.map(Descendant.Descender.traversal)(collection.breakOut),
      () ⇒ paths.flatMap(Descendant.Descender.ancestors)(collection.breakOut)
    )

    def compact: Json = filterNulls
    def filterNulls: Json = filterR(_ != jNull)
    def filterKeys(p: Predicate[String]): Json = self.withObject(_.filterKeys(p))
    def filterValues(p: Predicate[Json]): Json = self.withObject(_.filterValues(p))

    def renameField(from: String, to: String):    Json = self.withObject(_.renameField(from, to))
    def renameFields(fromTos: (String, String)*): Json = self.withObject(_.renameFields(fromTos: _*))

    def addIfMissing(name: String, value: Json): Json = self.withObject(_.addIfMissing(name, value))
    def addIfMissing(assocs: Json.JsonAssoc*):   Json = self.withObject(_.addIfMissing(assocs: _*))

    def removeIfPresent(name: String, value: Json): Json = self.withObject(_.removeIfPresent(name, value))
    def removeIfPresent(assocs: Json.JsonAssoc*): Json = self.withObject(_.removeIfPresent(assocs: _*))

    def removeFields(names: String*): Json = self.withObject(_.removeFields(names: _*))

//    def delete(path: String): Json = {
//      path.split("/").toList.reverse match {
//        case head :: Nil ⇒ descendant("").obj.delete(head)
//        case head :: tail ⇒ descendant(tail.reverse.mkString("/")).obj.delete(head)
//        case _ ⇒ Json.jNull
//      }
//    }

    def filterR(p: Predicate[Json]): Json =
      p.cond(self.withObject(_.filterR(p)).withArray(_.filterR(p)), jNull)(self)

    def writeTo(file: File): Json =
      self.tap(_ ⇒ file.writeString(indent2))

    def indent2: String =
      PrettyParams.spaces2.copy(preserveOrder = true).pretty(self)

    def append(keys: List[String], value: Json): Json = self.withObject(obj => {
      keys match {
        case Nil => obj
        case head :: Nil => obj + (head, value)
        case head :: tail => {
          val subObject = obj(head).getOrElse(Json.jEmptyObject)

          obj + (head, subObject.append(tail, value))
        }
      }
    })
    
    def pivot: List[(String, Json)] = {
      def recurse(path: String, current: Json): List[(String, Json)] = current match {
        case JsonMonocle.jObjectPrism(obj) => obj.toList.flatMap {
          case (r":$field", value) => recurse(s"$path:$field", value)
          case (field, value)        => recurse(s"$path/$field", value)
        }
        case other => List(path -> other)
      }

      recurse("", self).mapFirst(_.stripPrefix("/"))
    }
  }

  implicit class CodecJsonCompanionFrills(val self: CodecJson.type) extends AnyVal {
    def defer[A](deferred: ⇒ CodecJson[A]): CodecJson[A] =
      CodecJson.derived(EncodeJson.defer(deferred.Encoder), DecodeJson.defer(deferred.Decoder))
  }

  implicit class CodecJsonFrills[A](val self: CodecJson[A]) extends AnyVal {
    def renameField(from: String, to: String):      CodecJson[A] = afterEncode(_.renameField(from, to))
    def renameFields(fromTos: (String, String)*):   CodecJson[A] = afterEncode(_.renameFields(fromTos: _*))
    def addIfMissing(name: String, value: Json):    CodecJson[A] = afterEncode(_.addIfMissing(name, value))
    def addIfMissing(assocs: Json.JsonAssoc*):      CodecJson[A] = afterEncode(_.addIfMissing(assocs: _*))
    def removeIfPresent(name: String, value: Json): CodecJson[A] = afterEncode(_.removeIfPresent(name, value))
    def removeIfPresent(assocs: Json.JsonAssoc*):   CodecJson[A] = afterEncode(_.removeIfPresent(assocs: _*))
    def removeFields(names: String*):               CodecJson[A] = afterEncode(_.removeFields(names: _*))

    def beforeDecode(f: Json ⇒ Json): CodecJson[A] = compose(f)
    def afterDecode(f: A ⇒ A):        CodecJson[A] = derived(encoder ⇒ encoder)(_ map f)
    def beforeEncode(f: A ⇒ A):       CodecJson[A] = derived(_ contramap f)(decoder ⇒ decoder)
    def afterEncode(f: Json ⇒ Json):  CodecJson[A] = andThen(f)
    def andThen(f: Json ⇒ Json):      CodecJson[A] = derived(_ andThen f)(decoder ⇒ decoder)
    def compose(f: Json ⇒ Json):      CodecJson[A] = derived(encoder ⇒ encoder)(_ compose f)
    def xmapDisjunction[B](f: A ⇒ String \/ B)(g: B ⇒ A): CodecJson[B] = derived(_ beforeEncode g)(_ afterDecode f)

    def derived[B](f: EncodeJson[A] ⇒ EncodeJson[B])(g: DecodeJson[A] ⇒ DecodeJson[B]): CodecJson[B] =
      CodecJson.derived[B](f(self.Encoder), g(self.Decoder))

    def traversalToJson: Traversal[A, Json] = new Traversal[A, Json] {
      def modifyF[F[_]](f: Json ⇒ F[Json])(a: A)(implicit F: Applicative[F]): F[A] = {
        F.map[Json, A](f(self.encode(a)))(json ⇒ self.decodeJson(json).getOr(a))
      }
    }
    
    def wrapExceptions(name: String): CodecJson[A] = CodecJson.derived[A](
      EncodeJson[A](a => wrapExceptions(s"Encode($name)", self.encode(a))),
      DecodeJson[A](c => wrapExceptions(s"Decode($name)", self.decode(c)))
    )

    private def wrapExceptions[X](description: String, f: => X): X = try { f } catch {
      case ce: CodecException => throw description :: ce
      case e: Exception       => throw CodecException(List(description), e)
    }
  }

  implicit class CodecJsonMapFrills[K, V](val self: CodecJson[K ▶: V]) extends AnyVal {
    def xmapEntries[C, W](kvcw: (K, V) ⇒ (C, W))(cwkv: (C, W) ⇒ (K, V)): CodecJson[C ▶: W] =
      self.derived[C ▶: W](_ contramapEntries cwkv)(_ mapEntries kvcw)

    def xmapKeys[C](kc: K ⇒ C)(ck: C ⇒ K):   CodecJson[C ▶: V] = self.derived(_ contramapKeys ck)(_ mapKeys kc)
    def xmapValues[W](vw: V ⇒ W)(wv: W ⇒ V): CodecJson[K ▶: W] = self.derived(_ contramapValues wv)(_ mapValues vw)
  }

  implicit class DecodeJsonCompanionFrills(val self: DecodeJson.type) extends AnyVal {
    def defer[A](deferred: ⇒ DecodeJson[A]): DecodeJson[A] = new DecodeJson[A] {
      def decode(c: HCursor): DecodeResult[A] = _deferred.decode(c)

      private lazy val _deferred: DecodeJson[A] = deferred
    }
  }

  implicit class DecodeJsonFrills[A](val self: DecodeJson[A]) extends AnyVal {
    def renameField(from: String, to: String):      DecodeJson[A] = beforeDecode(_.renameField(from, to))
    def renameFields(fromTos: (String, String)*):   DecodeJson[A] = beforeDecode(_.renameFields(fromTos: _*))
    def addIfMissing(name: String, value: Json):    DecodeJson[A] = beforeDecode(_.addIfMissing(name, value))
    def addIfMissing(assocs: Json.JsonAssoc*):      DecodeJson[A] = beforeDecode(_.addIfMissing(assocs: _*))
    def removeIfPresent(name: String, value: Json): DecodeJson[A] = beforeDecode(_.removeIfPresent(name, value))
    def removeIfPresent(assocs: Json.JsonAssoc*):   DecodeJson[A] = beforeDecode(_.removeIfPresent(assocs: _*))
    def removeFields(names: String*):               DecodeJson[A] = beforeDecode(_.removeFields(names: _*))

    def beforeDecode(f: Json ⇒ Json): DecodeJson[A] = compose(f)
    def compose(f: Json ⇒ Json):      DecodeJson[A] = DecodeJson[A](hc ⇒ self.decode(hc >-> f))
    def upcast[B >: A]:               DecodeJson[B] = self.map[B](a ⇒ a: B)

    private[argonaut] def afterDecode[B](f: A ⇒ String \/ B): DecodeJson[B] = // Probably publish later
      DecodeJson[B](c ⇒ self.decode(c).flatMap(a ⇒ DecodeResult[B](f(a).leftMap(_ → c.history).toEither)))
  }

  implicit class DecodeJsonMapFrills[K, V](val self: DecodeJson[K ▶: V]) extends AnyVal {
    def mapEntries[C, W](f: (K, V) ⇒ (C, W)): DecodeJson[C ▶: W] =
      self.map(_.mapEntries(k ⇒ v ⇒ f(k, v)))

    def mapKeys[C](f: K ⇒ C):   DecodeJson[C ▶: V] = self.map(_.mapKeysEagerly(f))
    def mapValues[W](f: V ⇒ W): DecodeJson[K ▶: W] = self.map(_.mapValuesEagerly(f))
  }

  implicit class EncodeJsonCompanionFrills(val self: EncodeJson.type) extends AnyVal {
    def defer[A](deferred: ⇒ EncodeJson[A]): EncodeJson[A] = new EncodeJson[A] {
      def encode(a: A): Json = _deferred.encode(a)

      private lazy val _deferred: EncodeJson[A] = deferred
    }
  }

  implicit class EncodeJsonFrills[A](val self: EncodeJson[A]) extends AnyVal {
    def renameField(from: String, to: String):      EncodeJson[A] = afterEncode(_.renameField(from, to))
    def renameFields(fromTos: (String, String)*):   EncodeJson[A] = afterEncode(_.renameFields(fromTos: _*))
    def addIfMissing(name: String, value: Json):    EncodeJson[A] = afterEncode(_.addIfMissing(name, value))
    def addIfMissing(assocs: Json.JsonAssoc*):      EncodeJson[A] = afterEncode(_.addIfMissing(assocs: _*))
    def removeIfPresent(name: String, value: Json): EncodeJson[A] = afterEncode(_.removeIfPresent(name, value))
    def removeIfPresent(assocs: Json.JsonAssoc*):   EncodeJson[A] = afterEncode(_.removeIfPresent(assocs: _*))
    def removeFields(names: String*):               EncodeJson[A] = afterEncode(_.removeFields(names: _*))

    def afterEncode(f: Json ⇒ Json): EncodeJson[A] = andThen(f)
    def andThen(f: Json ⇒ Json):     EncodeJson[A] = EncodeJson[A](a ⇒ f(self.encode(a)))
    def downcast[B <: A]:            EncodeJson[B] = self.contramap[B](b ⇒ b: A)

    def add(assocsFn: (A ⇒ Json.JsonAssoc)*): EncodeJson[A] = {
      EncodeJson[A](a ⇒ self.encode(a).addIfMissing(assocsFn.map(assoc ⇒ assoc.apply(a)): _*))
    }

    private[argonaut] def beforeEncode[B](f: B ⇒ A): EncodeJson[B] = self contramap f // Probably publish later
  }

  implicit class EncodeJsonMapFrills[K, V](val self: EncodeJson[K ▶: V]) extends AnyVal {
    def contramapEntries[C, W](f: (C, W) ⇒ (K, V)): EncodeJson[C ▶: W] =
      self.contramap[C ▶: W](_.mapEntries(c ⇒ w ⇒ f(c, w)))

    def contramapKeys[C](f: C ⇒ K):   EncodeJson[C ▶: V] = self.contramap[C ▶: V](_.mapKeysEagerly(f))
    def contramapValues[W](f: W ⇒ V): EncodeJson[K ▶: W] = self.contramap[K ▶: W](_.mapValuesEagerly(f))
  }

  implicit class TraversalFrills[A, B](val self: Traversal[A, B]) extends AnyVal {
    def bool[That](  implicit cpf: CanPrismFrom[B, Boolean,    That]): Traversal[A, That] = apply(cpf)
    def number[That](implicit cpf: CanPrismFrom[B, JsonNumber, That]): Traversal[A, That] = apply(cpf)
    def string[That](implicit cpf: CanPrismFrom[B, String,     That]): Traversal[A, That] = apply(cpf)
    def array[That]( implicit cpf: CanPrismFrom[B, List[Json], That]): Traversal[A, That] = apply(cpf)
    def obj[That](   implicit cpf: CanPrismFrom[B, JsonObject, That]): Traversal[A, That] = apply(cpf)

    def double[That](    implicit cpf: CanPrismFrom[B, Double,     That]): Traversal[A, That] = apply(cpf)
    def int[That](       implicit cpf: CanPrismFrom[B, Int,        That]): Traversal[A, That] = apply(cpf)
    def float[That](     implicit cpf: CanPrismFrom[B, Float,      That]): Traversal[A, That] = apply(cpf)
    def short[That](     implicit cpf: CanPrismFrom[B, Short,      That]): Traversal[A, That] = apply(cpf)
    def byte[That](      implicit cpf: CanPrismFrom[B, Byte,       That]): Traversal[A, That] = apply(cpf)
    def bigDecimal[That](implicit cpf: CanPrismFrom[B, BigDecimal, That]): Traversal[A, That] = apply(cpf)
    def bigInt[That](    implicit cpf: CanPrismFrom[B, BigInt,     That]): Traversal[A, That] = apply(cpf)

    private def apply[Elem, That](canPrismFrom: CanPrismFrom[B, Elem, That]): Traversal[A, That] =
      self composePrism canPrismFrom.prism
  }

  implicit class JsonObjectFrills(val self: JsonObject) extends AnyVal {
    def filterKeys(p: Predicate[String]): JsonObject = mapCollect { case entry@(k, _) if p(k) ⇒ entry }
    def filterValues(p: Predicate[Json]): JsonObject = mapCollect { case entry@(_, j) if p(j) ⇒ entry }

    def removeFields(names: String*): JsonObject = {
      val namesSet: Set[String] = names.toSet

      mapCollect { case entry@(k, _) if !namesSet.contains(k) ⇒ entry }
    }

    def renameFields(fromTos: (String, String)*): JsonObject = fromTos.foldLeft(self) {
      case (acc, (from, to)) ⇒ acc.renameField(from, to)
    }

    def renameField(from: String, to: String): JsonObject =
      self(from).fold(self)(value ⇒ (self - from) + (to, value))


    def addIfMissing(assocs: Json.JsonAssoc*): JsonObject = assocs.foldLeft(self) {
      case (acc, (name, value)) ⇒ acc.addIfMissing(name, value)
    }

    def addIfMissing(name: String, value: Json): JsonObject =
      self(name).fold(self + (name, value))(_ ⇒ self)


    def removeIfPresent(assocs: Json.JsonAssoc*): JsonObject = assocs.foldLeft(self) {
      case (acc, (name, value)) ⇒ acc.removeIfPresent(name, value)
    }

    def removeIfPresent(name: String, value: Json): JsonObject =
      self(name).fold(self)(existing ⇒ if (existing == value) self - name else self)


    def filterR(p: Predicate[Json]): JsonObject =
      mapCollect { case (k, j) if p(j) ⇒ k -> j.filterR(p) }

    private def mapCollect(pf: (String, Json) ~> (String, Json)): JsonObject = {
      val from: ListMap[String, Json] = ListMap[String, Json](self.toList: _*)
      val to: ListMap[String, Json] = from.collect(pf)

      JsonObject.fromTraversableOnce(to)
    }
  }

  implicit class TraversalToJsonFrills[A](val self: Traversal[A, Json]) extends AnyVal {
    def renameField(from: String, to: String):    Traversal[A, Json] =
      self composeIso Iso[Json, Json](_.renameField(from, to))(_.renameField(to, from))

    def renameFields(fromTos: (String, String)*): Traversal[A, Json] =
      self composeIso Iso[Json, Json](_.renameFields(fromTos: _*))(_.renameFields(fromTos.map(_.swap): _*))

    def descendant(path: String): Traversal[A, Json] =
      Descendant.Descender(path).traversal(self, path)
  }

  implicit class JsonArrayFrills(val self: List[Json]) extends AnyVal {
    def filterR(p: Predicate[Json]): List[Json] = self.collect { case j if p(j) ⇒ j.filterR(p) }
  }
}

private case class CodecException(descriptions: List[String], cause: Exception) extends Exception("", cause) {
  setStackTrace(descriptions.map(new StackTraceElement(_, "", "", 0)).toArray)
  
  def ::(description: String): CodecException = copy(description :: descriptions)
}

case class CanPrismFrom[From, Elem, To](prism: Prism[From, To]) {
  def toList: CanPrismFrom[List[From], Elem, List[To]] =
    CanPrismFrom(Prism[List[From], List[To]](la ⇒ Some(la.flatMap(prism.getOption)))(_.map(prism.reverseGet)))

  def toMap[K]: CanPrismFrom[K ▶: From, Elem, K ▶: To] = CanPrismFrom(Prism[K ▶: From, K ▶: To](mapKA ⇒ {
    Some(mapKA.updateValues(a ⇒ prism.getOption(a)))
  })((mapKB: K ▶: To) ⇒ {
    mapKB.mapValuesEagerly(prism.reverseGet)
  }))
}

object CanPrismFrom {
  implicit val cpfJsonToBoolean:    CanPrismFrom[Json, Boolean,    Boolean]    = apply(JsonMonocle.jBoolPrism)
  implicit val cpfJsonToJsonNumber: CanPrismFrom[Json, JsonNumber, JsonNumber] = apply(JsonMonocle.jNumberPrism)
  implicit val cpfJsonToString:     CanPrismFrom[Json, String,     String]     = apply(JsonMonocle.jStringPrism)
  implicit val cpfJsonToJsonArray:  CanPrismFrom[Json, List[Json], List[Json]] = apply(JsonMonocle.jArrayPrism)
  implicit val cpfJsonToJsonObject: CanPrismFrom[Json, JsonObject, JsonObject] = apply(JsonMonocle.jObjectPrism)
  implicit val cpfJsonToBigDecimal: CanPrismFrom[Json, BigDecimal, BigDecimal] = apply(JsonMonocle.jBigDecimalPrism)
//  implicit val cpfJsonToDouble:     CanPrismFrom[Json, Double,     Double]     = apply(jDoublePrism)
//  implicit val cpfJsonToFloat:      CanPrismFrom[Json, Float,      Float]      = apply(jFloatPrism)
  implicit val cpfJsonToBigInt:     CanPrismFrom[Json, BigInt,     BigInt]     = apply(JsonMonocle.jBigIntPrism)
  implicit val cpfJsonToLong:       CanPrismFrom[Json, Long,       Long]       = apply(JsonMonocle.jLongPrism)
  implicit val cpfJsonToInt:        CanPrismFrom[Json, Int,        Int]        = apply(JsonMonocle.jIntPrism)
  implicit val cpfJsonToShort:      CanPrismFrom[Json, Short,      Short]      = apply(JsonMonocle.jShortPrism)
  implicit val cpfJsonToByte:       CanPrismFrom[Json, Byte,       Byte]       = apply(JsonMonocle.jBytePrism)

  implicit def cpfJsonToCodec[A: CodecJson]: CanPrismFrom[Json, A, A] = {
    val A = CodecJson.derived[A]

    apply(Prism[Json, A](json ⇒ A.decodeJson(json).toOption)(A.encode))
  }

  implicit def cpfl[From, Elem, To](implicit cpf: CanPrismFrom[From, Elem, To])
    : CanPrismFrom[List[From], Elem, List[To]] = cpf.toList

  implicit def cpfm[From, Elem, To](implicit cpf: CanPrismFrom[From, Elem, To])
    : CanPrismFrom[String ▶: From, Elem, String ▶: To] = cpf.toMap

  implicit def cpfJsonObjectToTypedMap[V](implicit cpf: CanPrismFrom[Json, V, V])
    : CanPrismFrom[JsonObject, V, String ▶: V] = apply(jsonObjectMapIso.composePrism(cpf.toMap[String].prism))

  private val jsonObjectMapIso: Iso[JsonObject, String ▶: Json] =
    Iso[JsonObject, String ▶: Json](_.toMap)(map ⇒ JsonObject.fromTraversableOnce(map))
}

object Descendant {
  import pimpathon.argonaut.{JsonFrills, JsonObjectFrills, TraversalFrills}

//  implicit def descendantAsApplyTraversal[From, Via, To](descendant: Descendant[From, Via, To]):
//    ApplyTraversal[From, From, To, To] = ApplyTraversal(descendant.from, descendant.traversal)

  implicit class DescendantToJsonFrills[From](private val self: Descendant[From, Json, Json]) {
    def renameField(from: String, to: String):    From = self.modify(_.renameField(from, to))
    def renameFields(fromTos: (String, String)*): From = self.modify(_.renameFields(fromTos: _*))

    def addIfMissing(name: String, value: Json): From = self.modify(_.addIfMissing(name, value))
    def addIfMissing(assocs: Json.JsonAssoc*):   From = self.modify(_.addIfMissing(assocs: _*))

    def removeIfPresent(name: String, value: Json): From = self.modify(_.removeIfPresent(name, value))
    def removeIfPresent(assocs: Json.JsonAssoc*): From = self.modify(_.removeIfPresent(assocs: _*))

    def removeFields(names: String*): From = self.modify(_.removeFields(names: _*))

    def each: Descendant[From, Json, Json] = self composeTraversal objectValuesOrArrayElements
  }

  implicit class DescendantToJsonObjectFrills[From](private val self: Descendant[From, Json, JsonObject]) {
    def renameField(from: String, to: String):    From = self.modify(_.renameField(from, to))
    def renameFields(fromTos: (String, String)*): From = self.modify(_.renameFields(fromTos: _*))

    def addIfMissing(name: String, value: Json): From = self.modify(_.addIfMissing(name, value))
    def addIfMissing(assocs: Json.JsonAssoc*):   From = self.modify(_.addIfMissing(assocs: _*))

    def removeIfPresent(name: String, value: Json): From = self.modify(_.removeIfPresent(name, value))
    def removeIfPresent(assocs: Json.JsonAssoc*): From = self.modify(_.removeIfPresent(assocs: _*))

    def removeFields(names: String*): From = self.modify(_.removeFields(names: _*))

    def each: Descendant[From, Json, Json] = self composeTraversal monocle.function.Each.each

    //    def delete(key: String): From = {
//      (descendant.traversal composeLens At.at(key)).set(None).apply(descendant.from)
//    }
  }

  implicit class DescendantViaJsonFrills[From, To](private val self: Descendant[From, Json, To]) {
    def firstEmptyAt: Option[String] = ancestorsList.collectFirst {
      case (path, Nil) ⇒ path
    }

    def ancestors: Json =
      Json.jObjectAssocList(ancestorsList.mapSecond(Json.jArray))

    private def ancestorsList: List[(String, List[Json])] =
      self.ancestorsFn().mapSecond(ancestor ⇒ ancestor.getAll(self.from))
  }

  object Descender {
    def apply(path: String): Descender = if (path.startsWith("$")) JsonPath else Pimpathon

    def traversal(path: String): Traversal[Json, Json] = traversal(Traversal.id[Json], path)
    def traversal[A](start: Traversal[A, Json], path: String): Traversal[A, Json] = apply(path).traversal(start, path)

    def ancestors(path: String): List[(String, Traversal[Json, Json])] = ancestors(Traversal.id[Json], path)
    def ancestors[A](start: Traversal[A, Json], path: String): List[(String, Traversal[A, Json])] = apply(path).ancestors(start, path)
  }

  sealed trait Descender {
    def descendant[A](from: A, start: Traversal[A, Json], path: String): Descendant[A, Json, Json] =
      Descendant(from, List(traversal(start, path)), () ⇒ ancestors(start, path))

    def traversal[A](from: Traversal[A, Json], path: String): Traversal[A, Json]
    def ancestors[A](from: Traversal[A, Json], path: String): List[(String, Traversal[A, Json])]
  }

  case object JsonPath extends Descender {
    def traversal[A](from: Traversal[A, Json], path: String): Traversal[A, Json] = {
      new Parser().compile(path) match {
        case Parser.Success(pathTokens, _) ⇒ new JsonPathIntegration().traversal(pathTokens, from)
        case Parser.NoSuccess(msg, _)      ⇒ sys.error(s"Could not parse json path: $path, $msg")
      }
    }

    def ancestors[A](from: Traversal[A, Json], path: String): List[(String, Traversal[A, Json])] = {
      new Parser().compile(path) match {
        case Parser.Success(pathTokens, _) ⇒ new JsonPathIntegration().ancestors(pathTokens, from)
        case Parser.NoSuccess(msg, _)      ⇒ sys.error(s"Could not parse json path: $path, $msg")
      }
    }

    private class JsonPathIntegration[A] {
      def traversal(tokens: List[PathToken], start: Traversal[A, Json]): Traversal[A, Json] = tokens.foldLeft(start)(step)

      def ancestors(tokens: List[PathToken], start: Traversal[A, Json]): List[(String, Traversal[A, Json])] = {
        val traversals = tokens.scanLeft(start)(step)

        anotate(tokens, traversals).tail
      }

      private def step(acc: Traversal[A, Json], token: PathToken): Traversal[A, Json] = token match {
        case RecursiveField(name)           ⇒ notSupported(s"RecursiveField($name)")
        case RootNode                       ⇒ acc
        case AnyField                       ⇒ acc.obj composeTraversal each
        case MultiField(names)              ⇒ acc.obj composeTraversal filterIndex(names.toSet: Set[String])
        case Field(name)                    ⇒ acc.obj composeTraversal filterIndex(Set(name))
        case RecursiveAnyField              ⇒ notSupported("RecursiveAnyField")
        case CurrentNode                    ⇒ acc
        case filterToken: FilterToken       ⇒ filterArrayOrObject(filterObject(filterTokenStep(filterToken)))(acc)
        case ArraySlice(None, None, 1)      ⇒ acc.array composeTraversal each
        case ArraySlice(begin, end, step)   ⇒ notSupported(s"ArraySlice($begin, $end, $step)")
        case ArrayRandomAccess(indecies)    ⇒ acc.array composeTraversal filterIndex(indecies.toSet: Set[Int])
        case RecursiveFilterToken(filter)   ⇒ notSupported(s"RecursiveFilterToken($filter)")
      }

      private def filterTokenStep(token: FilterToken): Predicate[Json] = token match {
        case ComparisonFilter(op, lhs, rhs)       ⇒ comparisonFilter(op, lhs, rhs)
        case BooleanFilter(AndOperator, lhs, rhs) ⇒ filterTokenStep(lhs) and filterTokenStep(rhs)
        case BooleanFilter(OrOperator, lhs, rhs)  ⇒ filterTokenStep(lhs) or filterTokenStep(rhs)
        case HasFilter(SubQuery(subTokens))       ⇒ hasFilter(subTokens)
      }

      private def anotate(tokens: List[PathToken], traversals: List[Traversal[A, Json]]): List[(String, Traversal[A, Json])] = {
        tokens.map(toString).inits.map(_.mkString("")).toList.reverse.zip(traversals)
      }

      private def toString(token: PathToken): String = token match {
        case RootNode                             ⇒ "$"
        case AnyField                             ⇒ ".*"
        case MultiField(names)                    ⇒ names.map(_.quoteWith('\'')).mkString(", ")
        case Field(name)                          ⇒ s".$name"
        case CurrentNode                          ⇒ "@"
        case ComparisonFilter(op, lhs, rhs)       ⇒ s"?(${toString(lhs)} ${toString(op)} ${toString(rhs)})"
        case HasFilter(SubQuery(subTokens))       ⇒ subTokens.map(toString).mkString("")
        case ArraySlice(None, None, 1)            ⇒ "[*]"
        case ArrayRandomAccess(indecies)          ⇒ indecies.mkString(", ")
        case BooleanFilter(AndOperator, lhs, rhs) ⇒ s"${toString(lhs)} && ${toString(rhs)}"
        case BooleanFilter(OrOperator, lhs, rhs)  ⇒ s"${toString(lhs)} || ${toString(rhs)}"
        case other                                ⇒ throw new MatchError(s"not implemented for: $other")
      }

      private def toString(op: ComparisonOperator): String = op match {
        case EqOperator          ⇒ "=="
        case NotEqOperator       ⇒ "!="
        case GreaterOrEqOperator ⇒ ">="
        case LessOperator        ⇒ "<"
        case LessOrEqOperator    ⇒ "<="
        case GreaterOperator     ⇒ ">"
      }

      private def toString(fv: FilterValue): String = fv match {
        case JPTrue           ⇒ "true"
        case JPFalse          ⇒ "false"
        case JPLong(value)    ⇒ value.toString
        case JPString(value)  ⇒ value.quoteWith('\'')
        case SubQuery(tokens) ⇒ tokens.map(toString).mkString("")
        case _      ⇒ sys.error(fv.toString)
      }

      private def comparisonFilter(op: ComparisonOperator, lhs: FilterValue, rhs: FilterValue): Predicate[Json] = (op, lhs, rhs) match {
        case (EqOperator, EQ(fn), value)            ⇒ fn(value)
        case (EqOperator, value, EQ(fn))            ⇒ fn(value)
        case (GreaterOrEqOperator, GTEQ(fn), value) ⇒ fn(value)
        case _                                      ⇒ notSupported((op, lhs, rhs))
      }

      // TODO make this fully recursive
      val EQ = ComparisonArgument {
        case SubQuery(List(CurrentNode))                              ⇒ toPredicate(rhs ⇒ _ == json(rhs))
        case SubQuery(List(CurrentNode, Field(name)))                 ⇒ toPredicate(rhs ⇒ _.field(name).contains(json(rhs)))
        case SubQuery(List(CurrentNode, Field(first), Field(second))) ⇒ toPredicate(rhs ⇒ _.fieldOrEmptyObject(first).field(second).contains(json(rhs)))
      }

      val GTEQ = {
        implicit val orderingJson: Ordering[Json] = {
          Ordering.Tuple4[Option[Boolean], Option[Int], Option[Double], Option[String]].on[Json](json ⇒ {
            (json.bool, json.number.flatMap(_.toInt), json.number.flatMap(_.toDouble), json.string)
          })
        }

        implicit def orderOps[B](a: B)(implicit O: Ordering[B]): O.Ops = O.mkOrderingOps(a)

        ComparisonArgument {
          case SubQuery(List(CurrentNode, Field(name))) ⇒ toPredicate(rhs ⇒ lhs ⇒ lhs.field(name) >= Some(json(rhs)))
          case other ⇒ notSupported(other)
        }
      }

      private def notSupported[X](x: X): Nothing = sys.error(s"$x not supported !")

      private def hasFilter(tokens: List[PathToken]): Predicate[Json] = tokens match {
        case List(CurrentNode, Field(name)) ⇒ _.hasField(name)
      }

      trait ComparisonArgument {
        def unapply(lhs: FilterValue): Option[FN]
      }

      object ComparisonArgument {
        def apply(pf: PartialFunction[FilterValue, FN]): ComparisonArgument = (lhs: FilterValue) ⇒ pf.lift(lhs)
      }

      type FN = FilterValue ⇒ Predicate[Json]

      private def toPredicate(f: FilterValue ⇒ Predicate[Json]): FN = f

      private def json(fdv: FilterValue): Json = fdv match {
        case JPTrue          ⇒ jTrue
        case JPFalse         ⇒ jFalse
        case JPDouble(value) ⇒ Json.jNumberOrNull(value)
        case JPLong(value)   ⇒ Json.jNumber(value)
        case JPString(value) ⇒ jString(value)
        case JPNull          ⇒ jNull
        case unknown         ⇒ sys.error(s"boom: $unknown")
      }

      def filterArrayOrObject(prism: Prism[Json, Json])(acc: Traversal[A, Json]): Traversal[A, Json] =
        acc composeTraversal objectValuesOrArrayElements composePrism prism
    }
  }

  case object Pimpathon extends Descender {
    def traversal[A](start: Traversal[A, Json], path: String): Traversal[A, Json] =
      path.split("/").toList.filter(_.nonEmpty).foldLeft(start)(step)

    def ancestors[A](start: Traversal[A, Json], path: String): List[(String, Traversal[A, Json])] = {
      val tokens:     List[String]             = path.split("/").toList.filter(_.nonEmpty)
      val traversals: List[Traversal[A, Json]] = tokens.scanLeft(start)(step)

      tokens.inits.map(_.mkString("/")).toList.reverse.zip(traversals)
    }
    
    import argonaut.RegexMatcher

    private def step[A](acc: Traversal[A, Json], token: String): Traversal[A, Json] = token match {
      case "*"                            ⇒ acc composeTraversal objectValuesOrArrayElements
      case r"""\*\[${key}='${value}'\]""" ⇒ acc.array composeTraversal each composePrism filterObject(key, jString(value))
      case r"""\[${Split(indices)}\]"""   ⇒ acc.array composeTraversal filterIndex(indices.map(_.toInt))
      case r"""\{${Split(keys)}\}"""      ⇒ acc.obj composeTraversal filterIndex(keys)
      case key                            ⇒ acc.obj composeTraversal filterIndex(Set(key))
    }

    private object Split { def unapply(value: String): Option[Set[String]] = Some(value.split(",").map(_.trim).toSet) }
  }

  private def filterObject(key: String, value: Json): Prism[Json, Json] =
    filterObject(_.field(key).contains(value))

  private def filterObject(p: Predicate[Json]): Prism[Json, Json] =
    Prism[Json, Json](json ⇒ p(json).option(json))(json ⇒ json)

  private final lazy val objectValuesOrArrayElements: Traversal[Json, Json] = new Traversal[Json, Json] {
    def modifyF[F[_]](f: Json ⇒ F[Json])(j: Json)(implicit F: Applicative[F]): F[Json] = j.fold(
      jsonNull   = F.pure(j), jsonBool = _ ⇒ F.pure(j), jsonNumber = _ ⇒ F.pure(j), jsonString = _ ⇒ F.pure(j),
      jsonArray  = arr ⇒ F.map(each[List[Json], Json].modifyF(f)(arr))(Json.array(_: _*)),
      jsonObject = obj ⇒ F.map(each[JsonObject, Json].modifyF(f)(obj))(Json.jObject)
    )
  }

  case class As[From, Via, To, A: CodecJson](from: Descendant[From, Via, To])

  object As {
    implicit def asToDescendant[From, Via, To, A, That](as: As[From, Via, To, A])
      (implicit cpf: CanPrismFrom[To, A, That]): Descendant[From, Via, That] = as.from.composePrism(cpf.prism)
  }
}

case class Descendant[From, Via, To](
  from: From, traversals: List[Traversal[From, To]], ancestorsFn: () ⇒ List[(String, Traversal[From, Via])]
) extends Dynamic {

  def bool[That](  implicit cpf: CanPrismFrom[To, Boolean,    That]): Descendant[From, Via, That] = apply(cpf)
  def number[That](implicit cpf: CanPrismFrom[To, JsonNumber, That]): Descendant[From, Via, That] = apply(cpf)
  def string[That](implicit cpf: CanPrismFrom[To, String,     That]): Descendant[From, Via, That] = apply(cpf)
  def array[That]( implicit cpf: CanPrismFrom[To, List[Json], That]): Descendant[From, Via, That] = apply(cpf)
  def obj[That](   implicit cpf: CanPrismFrom[To, JsonObject, That]): Descendant[From, Via, That] = apply(cpf)

  def double[That](    implicit cpf: CanPrismFrom[To, Double,     That]): Descendant[From, Via, That] = apply(cpf)
  def int[That](       implicit cpf: CanPrismFrom[To, Int,        That]): Descendant[From, Via, That] = apply(cpf)
  def float[That](     implicit cpf: CanPrismFrom[To, Float,      That]): Descendant[From, Via, That] = apply(cpf)
  def short[That](     implicit cpf: CanPrismFrom[To, Short,      That]): Descendant[From, Via, That] = apply(cpf)
  def byte[That](      implicit cpf: CanPrismFrom[To, Byte,       That]): Descendant[From, Via, That] = apply(cpf)
  def bigDecimal[That](implicit cpf: CanPrismFrom[To, BigDecimal, That]): Descendant[From, Via, That] = apply(cpf)
  def bigInt[That](    implicit cpf: CanPrismFrom[To, BigInt,     That]): Descendant[From, Via, That] = apply(cpf)

  def as[A: CodecJson]: Descendant.As[From, Via, To, A] = Descendant.As[From, Via, To, A](this)

  def selectDynamic(key: String)(implicit cpf: CanPrismFrom[To, JsonObject, JsonObject]): Descendant[From, Via, Json] =
    obj[JsonObject] composeTraversal filterIndex(Set(key))

  private def apply[Elem, That](cpf: CanPrismFrom[To, Elem, That]): Descendant[From, Via, That] = composePrism(cpf.prism)

  def composePrism[That](next: Prism[To, That]):         Descendant[From, Via, That] = withTraversal(_ composePrism next)
  def composeTraversal[That](next: Traversal[To, That]): Descendant[From, Via, That] = withTraversal(_ composeTraversal next)
  def composeOptional[That](next: Optional[To, That]):   Descendant[From, Via, That] = withTraversal(_ composeOptional next)
  def composeIso[That](next: Iso[To, That]):             Descendant[From, Via, That] = withTraversal(_ composeIso next)

  def headOption: Option[To] = traversals.flatMap(_.headOption(from)).headOption
  def getAll: List[To] = traversals.flatMap(_.getAll(from))

  def set(to: To):         From = foldLeft(_.set(to))
  def modify(f: To ⇒ To): From = foldLeft(_.modify(f))

  private def foldLeft(f: Traversal[From, To] ⇒ From ⇒ From): From = traversals.foldLeft(from) {
    case (acc, traversal) ⇒ f(traversal)(acc)
  }

  private def withTraversal[That](fn: Traversal[From, To] ⇒ Traversal[From, That]): Descendant[From, Via, That] = 
    copy(traversals = traversals.map(fn))
}

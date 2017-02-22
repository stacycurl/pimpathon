package pimpathon

import scala.language.{dynamics, higherKinds, implicitConversions}
import io.gatling.jsonpath.AST._
import io.gatling.jsonpath._
import _root_.argonaut.{CodecJson, DecodeJson, DecodeResult, EncodeJson, Json, JsonMonocle, JsonNumber, JsonObject}
import _root_.argonaut.Json.{jFalse, jNull, jString, jTrue}
import _root_.argonaut.JsonObjectMonocle.{jObjectEach, jObjectFilterIndex}

import scala.collection.immutable.{Map ⇒ ▶:}
import monocle.{Iso, Optional, Prism, Traversal}
import monocle.function.Each.each
import monocle.function.FilterIndex.filterIndex
import monocle.std.list.{listEach, listFilterIndex}
import monocle.syntax.ApplyTraversal
import pimpathon.boolean.BooleanPimps
import pimpathon.function.{Predicate, PredicatePimps}
import pimpathon.map.MapPimps

import _root_.scalaz.{Applicative, \/}


object argonaut {
  implicit class JsonFrills(val self: Json) extends AnyVal {
    def descendant: Descendant[Json, Json] = descendant("")
    def descendant(path: String): Descendant[Json, Json] = Descendant(self, Traversal.id[Json].descendant(path))
    def compact: Json = filterNulls
    def filterNulls: Json = filterR(_ != jNull)
    def filterKeys(p: Predicate[String]): Json = self.withObject(_.filterKeys(p))
    def filterValues(p: Predicate[Json]): Json = self.withObject(_.filterValues(p))

    def renameField(from: String, to: String):    Json = self.withObject(_.renameField(from, to))
    def renameFields(fromTos: (String, String)*): Json = self.withObject(_.renameFields(fromTos: _*))

    def addIfMissing(name: String, value: Json): Json = self.withObject(_.addIfMissing(name, value))
    def addIfMissing(assocs: Json.JsonAssoc*):   Json = self.withObject(_.addIfMissing(assocs: _*))

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
  }

  implicit class CodecJsonFrills[A](val self: CodecJson[A]) extends AnyVal {
    def renameField(from: String, to: String):    CodecJson[A] = afterEncode(_.renameField(from, to))
    def renameFields(fromTos: (String, String)*): CodecJson[A] = afterEncode(_.renameFields(fromTos: _*))
    def addIfMissing(name: String, value: Json):  CodecJson[A] = afterEncode(_.addIfMissing(name, value))
    def addIfMissing(assocs: Json.JsonAssoc*):    CodecJson[A] = afterEncode(_.addIfMissing(assocs: _*))
    def removeFields(names: String*):             CodecJson[A] = afterEncode(_.removeFields(names: _*))

    def beforeDecode(f: Json ⇒ Json): CodecJson[A] = compose(f)
    def afterDecode(f: A ⇒ A):        CodecJson[A] = derived(encoder ⇒ encoder)(_ map f)
    def beforeEncode(f: A ⇒ A):       CodecJson[A] = derived(_ contramap f)(decoder ⇒ decoder)
    def afterEncode(f: Json ⇒ Json):  CodecJson[A] = andThen(f)
    def andThen(f: Json ⇒ Json):      CodecJson[A] = derived(_ andThen f)(decoder ⇒ decoder)
    def compose(f: Json ⇒ Json):      CodecJson[A] = derived(encoder ⇒ encoder)(_ compose f)
    def xmapDisjunction[B](f: A => String \/ B)(g: B => A): CodecJson[B] = derived(_ beforeEncode g)(_ afterDecode f)

    private[argonaut] def derived[B](f: EncodeJson[A] ⇒ EncodeJson[B])(g: DecodeJson[A] ⇒ DecodeJson[B]) =
      CodecJson.derived[B](f(self.Encoder), g(self.Decoder))
  }

  implicit class CodecJsonMapFrills[K, V](val self: CodecJson[K ▶: V]) extends AnyVal {
    def xmapKeys[C](kc: K ⇒ C)(ck: C ⇒ K):   CodecJson[C ▶: V] = self.derived(_ contramapKeys ck)(_ mapKeys kc)
    def xmapValues[W](vw: V ⇒ W)(wv: W ⇒ V): CodecJson[K ▶: W] = self.derived(_ contramapValues wv)(_ mapValues vw)
  }

  implicit class DecodeJsonFrills[A](val self: DecodeJson[A]) extends AnyVal {
    def renameField(from: String, to: String):    DecodeJson[A] = beforeDecode(_.renameField(from, to))
    def renameFields(fromTos: (String, String)*): DecodeJson[A] = beforeDecode(_.renameFields(fromTos: _*))
    def addIfMissing(name: String, value: Json):  DecodeJson[A] = beforeDecode(_.addIfMissing(name, value))
    def addIfMissing(assocs: Json.JsonAssoc*):    DecodeJson[A] = beforeDecode(_.addIfMissing(assocs: _*))
    def removeFields(names: String*):             DecodeJson[A] = beforeDecode(_.removeFields(names: _*))

    def beforeDecode(f: Json ⇒ Json): DecodeJson[A] = compose(f)
    def compose(f: Json ⇒ Json):      DecodeJson[A] = DecodeJson[A](hc ⇒ self.decode(hc >-> f))
    def upcast[B >: A]:               DecodeJson[B] = self.map[B](a ⇒ a: B)

    private[argonaut] def afterDecode[B](f: A => String \/ B): DecodeJson[B] = // Probably publish later
      DecodeJson[B](c => self.decode(c).flatMap(a => DecodeResult[B](f(a).leftMap(_ → c.history).toEither)))
  }

  implicit class DecodeJsonMapFrills[K, V](val self: DecodeJson[K ▶: V]) extends AnyVal {
    def mapEntries[C, W](f: (K, V) => (C, W)): DecodeJson[C ▶: W] =
      self.map(_.mapEntries(k => v => f(k, v)))

    def mapKeys[C](f: K ⇒ C):   DecodeJson[C ▶: V] = self.map(_.mapKeysEagerly(f))
    def mapValues[W](f: V ⇒ W): DecodeJson[K ▶: W] = self.map(_.mapValuesEagerly(f))
  }

  implicit class EncodeJsonFrills[A](val self: EncodeJson[A]) extends AnyVal {
    def renameField(from: String, to: String):    EncodeJson[A] = afterEncode(_.renameField(from, to))
    def renameFields(fromTos: (String, String)*): EncodeJson[A] = afterEncode(_.renameFields(fromTos: _*))
    def addIfMissing(name: String, value: Json):  EncodeJson[A] = afterEncode(_.addIfMissing(name, value))
    def addIfMissing(assocs: Json.JsonAssoc*):    EncodeJson[A] = afterEncode(_.addIfMissing(assocs: _*))
    def removeFields(names: String*):             EncodeJson[A] = afterEncode(_.removeFields(names: _*))

    def afterEncode(f: Json ⇒ Json): EncodeJson[A] = andThen(f)
    def andThen(f: Json ⇒ Json):     EncodeJson[A] = EncodeJson[A](a ⇒ f(self.encode(a)))
    def downcast[B <: A]:            EncodeJson[B] = self.contramap[B](b ⇒ b: A)

    private[argonaut] def beforeEncode[B](f: B => A): EncodeJson[B] = self contramap f // Probably publish later
  }

  implicit class EncodeJsonMapFrills[K, V](val self: EncodeJson[K ▶: V]) extends AnyVal {
    def contramapEntries[C, W](f: (C, W) => (K, V)): EncodeJson[C ▶: W] =
      self.contramap[C ▶: W](_.mapEntries(c => w => f(c, w)))

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
    def filterKeys(p: Predicate[String]): JsonObject = mapMap(_.filterKeys(p))
    def filterValues(p: Predicate[Json]): JsonObject = mapMap(_.filterValues(p))
    def removeFields(names: String*): JsonObject = mapMap(_.filterKeysNot(names.toSet))

    def renameFields(fromTos: (String, String)*): JsonObject = fromTos.foldLeft(self) {
      case (acc, (from, to)) ⇒ acc.renameField(from, to)
    }

    def renameField(from: String, to: String): JsonObject =
      self(from).fold(self)(value ⇒ (self - from) + (to, value))


    def addIfMissing(assocs: Json.JsonAssoc*): JsonObject = assocs.foldLeft(self) {
      case (acc, (name, value)) ⇒ acc.addIfMissing(name, value)
    }

    def addIfMissing(name: String, value: Json): JsonObject =
      self(name).fold(self + (name, value))(_ => self)


    def filterR(p: Predicate[Json]): JsonObject =
      mapMap(_.collectValues { case j if p(j) ⇒ j.filterR(p) })

    private def mapMap(f: Map[String, Json] ⇒ Map[String, Json]): JsonObject =
      JsonObject.fromTraversableOnce(f(self.toMap))
  }

  implicit class TraversalToJsonFrills[A](val self: Traversal[A, Json]) extends AnyVal {
    def renameField(from: String, to: String):    Traversal[A, Json] =
      self composeIso Iso[Json, Json](_.renameField(from, to))(_.renameField(to, from))

    def renameFields(fromTos: (String, String)*): Traversal[A, Json] =
      self composeIso Iso[Json, Json](_.renameFields(fromTos: _*))(_.renameFields(fromTos.map(_.swap): _*))

    def descendant(path: String): Traversal[A, Json] =
      if (path.startsWith("$")) Descendant.JsonPath.descend(self, path) else Descendant.Pimpathon.descend(self, path)
  }

  implicit class JsonArrayFrills(val self: List[Json]) extends AnyVal {
    def filterR(p: Predicate[Json]): List[Json] = self.collect { case j if p(j) ⇒ j.filterR(p) }
  }
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
  import pimpathon.argonaut.TraversalFrills
  import pimpathon.argonaut.JsonFrills
  import pimpathon.argonaut.JsonObjectFrills

  implicit def descendantAsApplyTraversal[From, To](descendant: Descendant[From, To]):
    ApplyTraversal[From, From, To, To] = ApplyTraversal(descendant.from, descendant.traversal)

  implicit class DescendantToJsonFrills[From](self: Descendant[From, Json]) {
    def renameField(from: String, to: String):    From = self.modify(_.renameField(from, to))
    def renameFields(fromTos: (String, String)*): From = self.modify(_.renameFields(fromTos: _*))

    def addIfMissing(name: String, value: Json): From = self.modify(_.addIfMissing(name, value))
    def addIfMissing(assocs: Json.JsonAssoc*):   From = self.modify(_.addIfMissing(assocs: _*))

    def removeFields(names: String*): From = self.modify(_.removeFields(names: _*))

    def each: Descendant[From, Json] = self composeTraversal objectValuesOrArrayElements
  }

  implicit class DescendantToJsonObjectFrills[From](self: Descendant[From, JsonObject]) {
    def renameField(from: String, to: String):    From = self.modify(_.renameField(from, to))
    def renameFields(fromTos: (String, String)*): From = self.modify(_.renameFields(fromTos: _*))

    def addIfMissing(name: String, value: Json): From = self.modify(_.addIfMissing(name, value))
    def addIfMissing(assocs: Json.JsonAssoc*):   From = self.modify(_.addIfMissing(assocs: _*))

    def removeFields(names: String*): From = self.modify(_.removeFields(names: _*))

    def each: Descendant[From, Json] = self composeTraversal monocle.function.Each.each

    //    def delete(key: String): From = {
//      (descendant.traversal composeLens At.at(key)).set(None).apply(descendant.from)
//    }
  }

  sealed trait Descender {
    def descend[A](from: Traversal[A, Json], path: String): Traversal[A, Json]
  }

  case object JsonPath extends Descender {
    def descend[A](from: Traversal[A, Json], path: String): Traversal[A, Json] = {
      new Parser().compile(path) match {
        case Parser.Success(pathTokens, _) ⇒ new JsonPathIntegration().doIt(pathTokens, from)
        case Parser.NoSuccess(msg, _)      ⇒ sys.error(s"Could not parse json path: $path, $msg")
      }
    }

    private class JsonPathIntegration[A] {
      def doIt(tokens: List[PathToken], start: Traversal[A, Json]): Traversal[A, Json] = tokens.foldLeft(start) {
        case (acc, RecursiveField(name))           ⇒ notSupported(s"RecursiveField($name)")
        case (acc, RootNode)                       ⇒ acc
        case (acc, AnyField)                       ⇒ acc.obj composeTraversal each
        case (acc, MultiField(names))              ⇒ acc.obj composeTraversal filterIndex(names.toSet: Set[String])
        case (acc, Field(name))                    ⇒ acc.obj composeTraversal filterIndex(Set(name))
        case (acc, RecursiveAnyField)              ⇒ notSupported("RecursiveAnyField")
        case (acc, CurrentNode)                    ⇒ acc
        case (acc, ComparisonFilter(op, lhs, rhs)) ⇒ comparisonFilter(acc, op, lhs, rhs)
        case (acc, BooleanFilter(op, lhs, rhs))    ⇒ notSupported(s"BooleanFilter($op, $lhs, $rhs)")
        case (acc, HasFilter(SubQuery(subTokens))) ⇒ hasFilter(acc, subTokens)
        case (acc, ArraySlice(None, None, 1))      ⇒ acc.array composeTraversal each
        case (acc, ArraySlice(begin, end, step))   ⇒ notSupported(s"ArraySlice($begin, $end, $step)")
        case (acc, ArrayRandomAccess(indecies))    ⇒ acc.array composeTraversal filterIndex(indecies.toSet: Set[Int])
        case (acc, RecursiveFilterToken(filter))   ⇒ notSupported(s"RecursiveFilterToken($filter)")
      }

      private def comparisonFilter(acc: Traversal[A, Json], op: ComparisonOperator, lhs: FilterValue, rhs: FilterValue) = (op, lhs, rhs) match {
        case (EqOperator, EQ(fn), value)            ⇒ fn(value)(acc)
        case (EqOperator, value, EQ(fn))            ⇒ fn(value)(acc)
        case (GreaterOrEqOperator, GTEQ(fn), value) ⇒ fn(value)(acc)
        case _                                      ⇒ notSupported((op, lhs, rhs))
      }

      // TODO make this fully recursive
      val EQ = ComparisonArgument {
        case SubQuery(List(CurrentNode))                              ⇒ fn(rhs ⇒ filterArray(filterObject(_ == json(rhs))))
        case SubQuery(List(CurrentNode, Field(name)))                 ⇒ fn(rhs ⇒ filterArray(filterObject(name, json(rhs))))
        case SubQuery(List(CurrentNode, Field(first), Field(second))) ⇒ fn(rhs ⇒ filterArray(filterObject(j ⇒ {
          j.fieldOrEmptyObject(first).field(second).contains(json(rhs))
        })))
      }

      val GTEQ = {
        implicit val orderingJson: Ordering[Json] = {
          Ordering.Tuple4[Option[Boolean], Option[Int], Option[Double], Option[String]].on[Json](json ⇒ {
            (json.bool, json.number.flatMap(_.toInt), json.number.flatMap(_.toDouble), json.string)
          })
        }

        implicit def orderOps[B](a: B)(implicit O: Ordering[B]): O.Ops = O.mkOrderingOps(a)

        ComparisonArgument {
          case SubQuery(List(CurrentNode, Field(name))) ⇒ fn(rhs ⇒ filterArray(filterObject(lhs ⇒ lhs.field(name) >= Some(json(rhs)))))
          case other ⇒ notSupported(other)
        }
      }

      private def notSupported[X](x: X): Nothing = sys.error(s"$x not supported !")

      private def hasFilter(acc: Traversal[A, Json], tokens: List[PathToken]) = tokens match {
        case List(CurrentNode, Field(name)) ⇒ filterArray(filterObject(_.hasField(name)))(acc)
      }

      trait ComparisonArgument {
        def unapply(lhs: FilterValue): Option[FN]
      }

      object ComparisonArgument {
        def apply(pf: PartialFunction[FilterValue, FN]): ComparisonArgument = new ComparisonArgument {
          def unapply(lhs: FilterValue): Option[FN] = pf.lift(lhs)
        }
      }

      type FN = FilterValue ⇒ Traversal[A, Json] ⇒ Traversal[A, Json]

      private def fn(f: FN) = f

      private def json(fdv: FilterValue): Json = fdv match {
        case JPTrue          ⇒ jTrue
        case JPFalse         ⇒ jFalse
        case JPDouble(value) ⇒ Json.jNumberOrNull(value)
        case JPLong(value)   ⇒ Json.jNumber(value)
        case JPString(value) ⇒ jString(value)
        case JPNull          ⇒ jNull
        case unknown         ⇒ sys.error(s"boom: $unknown")
      }

      def filterArray(prism: Prism[Json, Json])(acc: Traversal[A, Json]): Traversal[A, Json] = acc.array composeTraversal each composePrism prism
    }
  }

  case object Pimpathon extends Descender {
    def descend[A](from: Traversal[A, Json], path: String): Traversal[A, Json] = path.split("/").filter(_.nonEmpty).foldLeft(from) {
      case (acc, "*")                            ⇒ acc composeTraversal objectValuesOrArrayElements
      case (acc, r"""\*\[${key}='${value}'\]""") ⇒ acc.array composeTraversal each composePrism filterObject(key, jString(value))
      case (acc, r"""\[${Split(indices)}\]""")   ⇒ acc.array composeTraversal filterIndex(indices.map(_.toInt))
      case (acc, r"""\{${Split(keys)}\}""")      ⇒ acc.obj   composeTraversal filterIndex(keys)
      case (acc, key)                            ⇒ acc.obj   composeTraversal filterIndex(Set(key))
    }

    private implicit class RegexMatcher(val self: StringContext) extends AnyVal { def r = self.parts.mkString("(.+)").r }
    private object Split { def unapply(value: String): Option[Set[String]] = Some(value.split(",").map(_.trim).toSet) }
  }

  private def filterObject(key: String, value: Json): Prism[Json, Json] =
    filterObject(_.field(key).contains(value))

  private def filterObject(p: Predicate[Json]): Prism[Json, Json] =
    Prism[Json, Json](json ⇒ p(json).option(json))(json ⇒ json)

  private final lazy val objectValuesOrArrayElements: Traversal[Json, Json] = new Traversal[Json, Json] {
    def modifyF[F[_]](f: Json => F[Json])(j: Json)(implicit F: Applicative[F]): F[Json] = j.fold(
      jsonNull   = F.pure(j), jsonBool = _ => F.pure(j), jsonNumber = _ => F.pure(j), jsonString = _ => F.pure(j),
      jsonArray  = arr => F.map(each[List[Json], Json].modifyF(f)(arr))(Json.array(_: _*)),
      jsonObject = obj => F.map(each[JsonObject, Json].modifyF(f)(obj))(Json.jObject)
    )
  }
}

case class Descendant[From, To](from: From, traversal: Traversal[From, To]) extends Dynamic {
  def bool[That](  implicit cpf: CanPrismFrom[To, Boolean,    That]): Descendant[From, That] = apply(cpf)
  def number[That](implicit cpf: CanPrismFrom[To, JsonNumber, That]): Descendant[From, That] = apply(cpf)
  def string[That](implicit cpf: CanPrismFrom[To, String,     That]): Descendant[From, That] = apply(cpf)
  def array[That]( implicit cpf: CanPrismFrom[To, List[Json], That]): Descendant[From, That] = apply(cpf)
  def obj[That](   implicit cpf: CanPrismFrom[To, JsonObject, That]): Descendant[From, That] = apply(cpf)

  def double[That](    implicit cpf: CanPrismFrom[To, Double,     That]): Descendant[From, That] = apply(cpf)
  def int[That](       implicit cpf: CanPrismFrom[To, Int,        That]): Descendant[From, That] = apply(cpf)
  def float[That](     implicit cpf: CanPrismFrom[To, Float,      That]): Descendant[From, That] = apply(cpf)
  def short[That](     implicit cpf: CanPrismFrom[To, Short,      That]): Descendant[From, That] = apply(cpf)
  def byte[That](      implicit cpf: CanPrismFrom[To, Byte,       That]): Descendant[From, That] = apply(cpf)
  def bigDecimal[That](implicit cpf: CanPrismFrom[To, BigDecimal, That]): Descendant[From, That] = apply(cpf)
  def bigInt[That](    implicit cpf: CanPrismFrom[To, BigInt,     That]): Descendant[From, That] = apply(cpf)

  def selectDynamic(key: String)(implicit cpf: CanPrismFrom[To, JsonObject, JsonObject]): Descendant[From, Json] =
    obj[JsonObject] composeTraversal filterIndex(Set(key))

  private def apply[Elem, That](cpf: CanPrismFrom[To, Elem, That]): Descendant[From, That] = composePrism(cpf.prism)

  def composePrism[That](next: Prism[To, That]):         Descendant[From, That] = withTraversal(traversal composePrism next)
  def composeTraversal[That](next: Traversal[To, That]): Descendant[From, That] = withTraversal(traversal composeTraversal next)
  def composeOptional[That](next: Optional[To, That]):   Descendant[From, That] = withTraversal(traversal composeOptional next)
  def composeIso[That](next: Iso[To, That]):             Descendant[From, That] = withTraversal(traversal composeIso next)

  private def withTraversal[That](value: Traversal[From, That]) = copy(traversal = value)
}
package pimpathon

import io.gatling.jsonpath.AST._
import io.gatling.jsonpath._
import monocle.function.At

import scala.language.{higherKinds, implicitConversions}

import _root_.argonaut.{CodecJson, DecodeJson, DecodeResult, EncodeJson, Json, JsonMonocle, JsonNumber, JsonObject}
import _root_.argonaut.Json.{jNull, jString, jTrue, jFalse}
import _root_.argonaut.JsonObjectMonocle.{jObjectEach, jObjectFilterIndex}

import monocle.{Iso, Prism, Traversal}
import monocle.function.Each.each
import monocle.function.FilterIndex.filterIndex
import monocle.std.list.{listEach, listFilterIndex}
import monocle.syntax.ApplyTraversal

import pimpathon.boolean.BooleanPimps
import pimpathon.function.{Predicate, PredicatePimps}
import pimpathon.map.MapPimps

import _root_.scalaz.{Applicative, \/}


object argonaut {
  implicit class JsonFrills(val value: Json) extends AnyVal {
    def descendant(path: String): Descendant[Json, Json] = Descendant(value, Traversal.id[Json].descendant(path))
    def compact: Json = filterNulls
    def filterNulls: Json = filterR(_ != jNull)

    def renameField(from: String, to: String): Json = value.withObject(_.renameField(from, to))
    def renameFields(fromTos: (String, String)*): Json = value.withObject(_.renameFields(fromTos: _*))

    def addIfMissing(name: String, value: Json): Json = this.value.withObject(_.addIfMissing(name, value))

//    def delete(path: String): Json = {
//      path.split("/").toList.reverse match {
//        case head :: Nil ⇒ descendant("").obj.delete(head)
//        case head :: tail ⇒ descendant(tail.reverse.mkString("/")).obj.delete(head)
//        case _ ⇒ Json.jNull
//      }
//    }

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
      DecodeJson[B](c => value.decode(c).flatMap(a => DecodeResult[B](f(a).leftMap(_ → c.history).toEither)))
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
      traversal composePrism canPrismFrom.prism
  }

  implicit class JsonObjectFrills(val o: JsonObject) extends AnyVal {
    def renameFields(fromTos: (String, String)*): JsonObject = fromTos.foldLeft(o) {
      case (acc, (from, to)) ⇒ acc.renameField(from, to)
    }

    def renameField(from: String, to: String): JsonObject = o(from).fold(o)(value ⇒ (o - from) + (to, value))

    def addIfMissing(name: String, value: Json): JsonObject =
      o(name).fold(o + (name, value))(_ => o)

    private[argonaut] def filterR(p: Predicate[Json]): JsonObject =
      JsonObject.fromTraversableOnce(o.toMap.collectValues { case j if p(j) ⇒ j.filterR(p) })
  }

  implicit class TraversalToJsonFrills[A](val traversal: Traversal[A, Json]) extends AnyVal {
    def descendant(path: String): Traversal[A, Json] =
      if (path.startsWith("$")) Descendant.JsonPath.descend(traversal, path) else Descendant.Pimpathon.descend(traversal, path)
  }

  private implicit class JsonArrayFrills(val a: List[Json]) extends AnyVal {
    private[argonaut] def filterR(p: Predicate[Json]): List[Json] = a.collect { case j if p(j) ⇒ j.filterR(p) }
  }
}


case class CanPrismFrom[From, Elem, To](prism: Prism[From, To]) {
  def toList: CanPrismFrom[List[From], Elem, List[To]] =
    CanPrismFrom(Prism[List[From], List[To]](la ⇒ Some(la.flatMap(prism.getOption)))(_.map(prism.reverseGet)))

  def toMap[K]: CanPrismFrom[Map[K, From], Elem, Map[K, To]] = CanPrismFrom(Prism[Map[K, From], Map[K, To]](mapKA ⇒ {
    Some(mapKA.updateValues(a ⇒ prism.getOption(a)))
  })((mapKB: Map[K, To]) ⇒ {
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
    : CanPrismFrom[Map[String, From], Elem, Map[String, To]] = cpf.toMap

  implicit def cpfJsonObjectToTypedMap[V](implicit cpf: CanPrismFrom[Json, V, V])
    : CanPrismFrom[JsonObject, V, Map[String, V]] = apply(jsonObjectMapIso.composePrism(cpf.toMap[String].prism))

  private val jsonObjectMapIso: Iso[JsonObject, Map[String, Json]] =
    Iso[JsonObject, Map[String, Json]](_.toMap)(map ⇒ JsonObject.fromTraversableOnce(map))
}

object Descendant {
  import pimpathon.argonaut.TraversalFrills
  import pimpathon.argonaut.JsonFrills
  import pimpathon.argonaut.JsonObjectFrills

  implicit def descendantAsApplyTraversal[From, To](descendant: Descendant[From, To]):
    ApplyTraversal[From, From, To, To] = ApplyTraversal(descendant.from, descendant.traversal)

  implicit class DescendantToJsonFrills[From](descendant: Descendant[From, Json]) {
    def renameField(from: String, to: String): From = descendant.modify(_.renameField(from, to))
    def renameFields(fromTos: (String, String)*): From = descendant.modify(_.renameFields(fromTos: _*))
  }

  implicit class DescendantToJsonObjectFrills[From](descendant: Descendant[From, JsonObject]) {
    def renameField(from: String, to: String): From = descendant.modify(_.renameField(from, to))
    def renameFields(fromTos: (String, String)*): From = descendant.modify(_.renameFields(fromTos: _*))

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

        implicit def orderOps[B](a: B)(implicit O: Ordering[B]) = O.mkOrderingOps(a)

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

    private final lazy val objectValuesOrArrayElements: Traversal[Json, Json] = new Traversal[Json, Json] {
      def modifyF[F[_]](f: Json => F[Json])(j: Json)(implicit F: Applicative[F]): F[Json] = j.fold(
        jsonNull   = F.pure(j), jsonBool = _ => F.pure(j), jsonNumber = _ => F.pure(j), jsonString = _ => F.pure(j),
        jsonArray  = arr => F.map(each[List[Json], Json].modifyF(f)(arr))(Json.array(_: _*)),
        jsonObject = obj => F.map(each[JsonObject, Json].modifyF(f)(obj))(Json.jObject)
      )
    }

    private implicit class RegexMatcher(val sc: StringContext) extends AnyVal { def r = sc.parts.mkString("(.+)").r }
    private object Split { def unapply(value: String): Option[Set[String]] = Some(value.split(",").map(_.trim).toSet) }
  }

  private def filterObject(key: String, value: Json): Prism[Json, Json] =
    filterObject(_.field(key).contains(value))

  private def filterObject(p: Json ⇒ Boolean): Prism[Json, Json] =
    Prism[Json, Json](json ⇒ p(json).option(json))(json ⇒ json)
}

case class Descendant[From, To](from: From, traversal: Traversal[From, To]) {
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

  private def apply[Elem, That](cpf: CanPrismFrom[To, Elem, That]): Descendant[From, That] =
    copy(traversal = traversal composePrism cpf.prism)
}
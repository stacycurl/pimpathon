package pimpathon.argonautTests

import _root_.argonaut.Json._
import _root_.argonaut._
import pimpathon.option._

import scala.collection.immutable.{Map ⇒ ▶:}


trait JsonUtil {
  def reverse(json: Json): Json = json.withArray(_.reverse)
  def reverse[A](decodeResult: DecodeResult[List[A]]): DecodeResult[List[A]] = decodeResult.map(_.reverse)

  val codec: CodecJson[List[String]]        = CodecJson.derived[List[String]]
  val mapCodec: CodecJson[String ▶: String] = CodecJson.derived[String ▶: String]
  val stringCodec: CodecJson[String]        = CodecJson.derived[String]
  val (encoder, decoder)       = (codec.Encoder, codec.Decoder)
  val (mapEncoder, mapDecoder) = (mapCodec.Encoder, mapCodec.Decoder)

  val list = List("food", "foo", "bard", "bar")
  val json = Json.jArray(list.map(Json.jString))
  def jsonMap(kvs: (String, String)*) = Json.jObjectAssocList(kvs.map { case (k, v) ⇒ (k, Json.jString(v)) }.toList)

  trait Base
  object Base { val encoder = EncodeJson[Base]({ case d: Derived ⇒ Derived.codec.encode(d) }) }

  case class Derived(i: Int) extends Base
  object Derived { implicit val codec: CodecJson[Derived] = CodecJson.casecodec1(Derived.apply, Derived.unapply)("i") }

  val derived = Derived(123)
  val derivedEncoded = Derived.codec.encode(derived)

  val bananaMan: BananaMan = BananaMan(
    name = "Eric",
    age = 3,
    lying = true,
    address = Address(List("29 Acacia Road", "Nuttytown")),
    preferences = Preferences(bananas = true),
    width = 33.5,
    potatoes = Nil,
    knownUnknowns = KnownUnknowns(),
    awkward = Awkward(`1` = "one")
  )

  val acaciaRoad = bananaMan.address.lines.map(jString)
  val bananas = JsonObject.empty + ("bananas", jBool(bananaMan.preferences.bananas))
  val intObj = JsonObject.empty + ("1", jString(bananaMan.awkward.`1`))

  val fields@List(lying, name, address, age, width, preferences, potatoes, knownUnknowns, awkward) = List(
    jBool(bananaMan.lying), jString(bananaMan.name), jArray(acaciaRoad), jNumber(bananaMan.age), jNumberOrNull(bananaMan.width),
    jObject(bananas), jArrayElements(), jObjectFields(), jObject(intObj)
  )

  val jobj: Json = jObjectFields(
    "name" → name, "age" → age, "lying" → lying, "address" → address, "preferences" → preferences, "width" → width,
    "potatoes" → potatoes, "knownUnknowns" → knownUnknowns, "awkward" → awkward
  )

  case class BananaMan(
    name: String,
    age: Int,
    lying: Boolean,
    address: Address,
    preferences: Preferences,
    width: Double,
    potatoes: List[String],
    knownUnknowns: KnownUnknowns,
    awkward: Awkward
  )

  object BananaMan {
    implicit val bananaManCodec: CodecJson[BananaMan] =
      CodecJson.casecodec9(BananaMan.apply _, BananaMan.unapply _)(
        "name", "age", "lying", "address", "preferences", "width", "potatoes", "knownUnknowns", "awkward"
      )
  }

  case class Awkward(`1`: String)

  object Awkward {
    implicit val awkwardCodec: CodecJson[Awkward] =
      CodecJson.casecodec1(Awkward.apply _, Awkward.unapply _)("1")
  }

  case class KnownUnknowns()

  object KnownUnknowns {
    implicit val KnownUnknownsCodec: CodecJson[KnownUnknowns] = CodecJson.derived[KnownUnknowns](
      EncodeJson[KnownUnknowns](_ ⇒ Json.jEmptyObject),
      DecodeJson[KnownUnknowns](cursor ⇒ {
        if (cursor.focus == Json.jEmptyObject) {
          DecodeResult.ok(KnownUnknowns())
        } else {
          DecodeResult.fail("Not an empty object", cursor.history)
        }
      })
    )
  }

  case class Preferences(bananas: Boolean)

  object Preferences {
    implicit val preferencesCodec: CodecJson[Preferences] =
      CodecJson.casecodec1(Preferences.apply _, Preferences.unapply _)("bananas")
  }

  case class Address(lines: List[String]) {
    def reverse: Address = copy(lines.reverse)
  }

  object Address {
    implicit val addressCodec: CodecJson[Address] =
      CodecJson.derived[List[String]].xmap[Address](Address(_))(_.lines)
  }

  val redacted = jString("redacted")

  def parse(jsonText: String) = Parse.parseOption(jsonText).getOrThrow("not json")

  def obj(socks: Json.JsonAssoc*): Json = Json.jObjectFields(socks: _*)

  def reverseEntry(key: String, value: String): (String, String) = (key.reverse, value.reverse)
}
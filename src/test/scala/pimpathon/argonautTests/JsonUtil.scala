package pimpathon.argonautTests

import _root_.argonaut.Json._
import _root_.argonaut.JsonIdentity.ToJsonIdentity
import _root_.argonaut._
import pimpathon.option._


trait JsonUtil {
  def reverse(json: Json): Json = json.withArray(_.reverse)
  def reverse[A](decodeResult: DecodeResult[List[A]]): DecodeResult[List[A]] = decodeResult.map(_.reverse)

  val codec: CodecJson[List[String]]           = CodecJson.derived[List[String]]
  val mapCodec: CodecJson[Map[String, String]] = CodecJson.derived[Map[String, String]]
  val stringCodec: CodecJson[String]           = CodecJson.derived[String]
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

  val acaciaRoad = List(jString("29 Acacia Road"), jString("Nuttytown"))
  val bananas = JsonObject.empty + ("bananas", jBool(true))
  val intObj = JsonObject.empty + ("1", jString("one"))

  val fields@List(lying, name, address, age, width, preferences, potatoes, knownUnknowns, awkward) = List(
    jBool(true), jString("Eric"), jArray(acaciaRoad), jNumber(3), jNumberOrNull(33.5), jObject(bananas),
    jArrayElements(), jObjectFields(), jObject(intObj)
  )

  val jobj: Json = jObjectFields(
    "name" → name, "age" → age, "lying" → lying, "address" → address, "preferences" → preferences, "width" → width,
    "potatoes" → potatoes, "knownUnknowns" → knownUnknowns, "awkward" → awkward
  )

  val redacted = jString("redacted")

  def parse(jsonText: String) = Parse.parseOption(jsonText).getOrThrow("not json")

  def obj[V: EncodeJson](kvs: (String, V)*): Json = Json.jObjectFields(kvs.map { case (k, v) ⇒ (k, v.asJson) }: _*)
}
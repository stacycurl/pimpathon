package pimpathon.argonaut

import argonaut.{DecodeResult, CodecJson, EncodeJson, Json, Parse}
import org.junit.Test

import org.junit.Assert._
import pimpathon.any._
import pimpathon.map._
import pimpathon.option._
import pimpathon.argonaut.json._



class JsonTest {
  @Test def filterNulls(): Unit = test(_.filterNulls,
    """null"""                        → """null""",
    """{ "a": null, "b": 3 }"""       → """{ "b": 3 }""",
    """[ "a", null, "b" ]"""          → """[ "a", "b" ]""",
    """{ "o": [ "a", null, "b" ] }""" → """{ "o": [ "a", "b" ] }""",
    """[ { "a": null, "b": 3 } ]"""   → """[ { "b": 3 } ]"""
  )

  private def test(f: Json ⇒ Json, data: (String, String)*): Unit = data.foreach {
    case (input, expected) ⇒ assertEquals(parse(expected), f(parse(input)))
  }

  private def parse(content: String): Json = Parse.parseOption(content).getOrThrow("Invalid json in test\n" + content)
}

class CodecJsonTest extends JsonUtil {
  @Test def andThen(): Unit = assertEquals(reverse(codec.encode(list)),     codec.andThen(reverse).encode(list))
  @Test def compose(): Unit = assertEquals(codec.decodeJson(reverse(json)), codec.compose(reverse).decodeJson(json))

  @Test def xmapKeys(): Unit = mapCodec.xmapKeys[String](_.reverse)(_.reverse).calc(reversed ⇒ {
    assertEquals(mapCodec.encode(Map("oof" → "bar")), reversed.encode(Map("foo" → "bar")))
    assertEquals(mapCodec.decodeJson(jsonMap("oof" → "bar")), reversed.decodeJson(jsonMap("foo" → "bar")))
  })
}

class EncodeJsonTest extends JsonUtil {
  @Test def andThen(): Unit = assertEquals(reverse(encoder.encode(list)), encoder.andThen(reverse).encode(list))
  @Test def downcast(): Unit = assertEquals(derivedEncoded, Base.encoder.downcast[Derived].encode(derived))

  @Test def contramapKeys(): Unit = assertEquals(
    mapEncoder.encode(Map("oof" → "bar")),
    mapEncoder.contramapKeys[String](_.reverse).encode(Map("foo" → "bar"))
  )

  @Test def contramapValues(): Unit = assertEquals(
    mapEncoder.encode(Map("foo" → "rab")),
    mapEncoder.contramapValues[String](_.reverse).encode(Map("foo" → "bar"))
  )
}

class DecodeJsonTest extends JsonUtil {
  @Test def compose(): Unit = assertEquals(decoder.decodeJson(reverse(json)), decoder.compose(reverse).decodeJson(json))
  @Test def upcast(): Unit = assertEquals(DecodeResult.ok(derived), Derived.codec.upcast[Base].decodeJson(derivedEncoded))

  @Test def mapKeys(): Unit = assertEquals(
    mapDecoder.decodeJson(jsonMap("oof" → "bar")),
    mapDecoder.mapKeys(_.reverse).decodeJson(jsonMap("foo" → "bar"))
  )

  @Test def mapValues(): Unit = assertEquals(
    mapDecoder.decodeJson(jsonMap("foo" → "rab")),
    mapDecoder.mapValues(_.reverse).decodeJson(jsonMap("foo" → "bar"))
  )
}

trait JsonUtil {
  def reverse(json: Json): Json = json.withArray(_.reverse)

  val codec: CodecJson[List[String]] = CodecJson.derived[List[String]]
  val (encoder, decoder) = (codec.Encoder, codec.Decoder)
  val list = List("food", "foo", "bard", "bar")
  val json = Json.jArray(list.map(Json.jString))
  def jsonMap(kvs: (String, String)*): Json = Json.jObjectAssocList(kvs.toMap.mapValuesEagerly(Json.jString).toList)
  val mapCodec: CodecJson[Map[String, String]] = CodecJson.derived[Map[String, String]]
  val (mapEncoder, mapDecoder) = (mapCodec.Encoder, mapCodec.Decoder)

  trait Base
  object Base { val encoder = EncodeJson[Base]({ case d: Derived ⇒ Derived.codec.encode(d) }) }

  case class Derived(i: Int) extends Base
  object Derived { implicit val codec: CodecJson[Derived] = CodecJson.casecodec1(Derived.apply, Derived.unapply)("i") }

  val derived = Derived(123)
  val derivedEncoded = Derived.codec.encode(derived)
}
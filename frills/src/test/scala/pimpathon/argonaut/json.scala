package pimpathon.argonaut

import argonaut.{DecodeResult, CodecJson, EncodeJson, Json, Parse}
import org.junit.Test

import pimpathon.any._
import pimpathon.option._
import pimpathon.util._
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
    case (input, expected) ⇒ f(parse(input)) === parse(expected)
  }

  private def parse(content: String): Json = Parse.parseOption(content).getOrThrow("Invalid json in test\n" + content)
}

class CodecJsonTest extends JsonUtil {
  @Test def andThen(): Unit     = codec.andThen(reverse).encode(list)           === reverse(codec.encode(list))
  @Test def compose(): Unit     = codec.compose(reverse).decodeJson(json)       === codec.decodeJson(reverse(json))

  @Test def xmapKeys(): Unit = mapCodec.xmapKeys[String](_.reverse)(_.reverse).calc(reversed ⇒ {
    reversed.encode(Map("foo" → "bar"))         === mapCodec.encode(Map("oof" → "bar"))
    reversed.decodeJson(jsonMap("foo" → "bar")) === mapCodec.decodeJson(jsonMap("oof" → "bar"))
  })

  @Test def xmapValues(): Unit = mapCodec.xmapValues[String](_.reverse)(_.reverse).calc(reversed ⇒ {
    reversed.encode(Map("foo" → "bar"))         === mapCodec.encode(Map("foo" → "rab"))
    reversed.decodeJson(jsonMap("foo" → "bar")) === mapCodec.decodeJson(jsonMap("foo" → "rab"))
  })
}

class EncodeJsonTest extends JsonUtil {
  @Test def andThen(): Unit = encoder.andThen(reverse).encode(list) === reverse(encoder.encode(list))
  @Test def downcast(): Unit = Base.encoder.downcast[Derived].encode(derived) === derivedEncoded

  @Test def contramapKeys(): Unit =
    mapEncoder.contramapKeys[String](_.reverse).encode(Map("foo" → "bar")) === mapEncoder.encode(Map("oof" → "bar"))

  @Test def contramapValues(): Unit =
    mapEncoder.contramapValues[String](_.reverse).encode(Map("foo" → "bar")) === mapEncoder.encode(Map("foo" → "rab"))
}

class DecodeJsonTest extends JsonUtil {
  @Test def compose(): Unit = decoder.compose(reverse).decodeJson(json) === decoder.decodeJson(reverse(json))
  @Test def upcast(): Unit = Derived.codec.upcast[Base].decodeJson(derivedEncoded) === DecodeResult.ok(derived)

  @Test def mapKeys(): Unit =
    mapDecoder.mapKeys(_.reverse).decodeJson(jsonMap("foo" → "bar")) === mapDecoder.decodeJson(jsonMap("oof" → "bar"))

  @Test def mapValues(): Unit =
    mapDecoder.mapValues(_.reverse).decodeJson(jsonMap("foo" → "bar")) === mapDecoder.decodeJson(jsonMap("foo" → "rab"))
}

trait JsonUtil {
  def reverse(json: Json): Json = json.withArray(_.reverse)
  def reverse[A](decodeResult: DecodeResult[List[A]]): DecodeResult[List[A]] = decodeResult.map(_.reverse)

  val codec: CodecJson[List[String]]           = CodecJson.derived[List[String]]
  val mapCodec: CodecJson[Map[String, String]] = CodecJson.derived[Map[String, String]]
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
}
package pimpathon.argonaut

import argonaut._
import argonaut.Json.{jString, jNumber}
import monocle.Traversal
import org.junit.Test

import pimpathon.any._
import pimpathon.either._
import pimpathon.option._
import pimpathon.util._
import pimpathon.argonaut.json._
import pimpathon.pimpTry._
import pimpathon.frills.any._
import pimpathon.frills.pimpTry._

import scalaz.{-\/, \/-, \/}


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
  @Test def beforeDecode(): Unit = codec.beforeDecode(reverse).decodeJson(json)  === codec.decodeJson(reverse(json))
  @Test def afterDecode(): Unit  = codec.afterDecode(_.reverse).decodeJson(json) === reverse(codec.decodeJson(json))
  @Test def beforeEncode(): Unit = codec.beforeEncode(_.reverse).encode(list)    === codec.encode(list.reverse)
  @Test def afterEncode(): Unit  = codec.afterEncode(reverse).encode(list)       === reverse(codec.encode(list))
  @Test def andThen(): Unit      = codec.andThen(reverse).encode(list)           === reverse(codec.encode(list))
  @Test def compose(): Unit      = codec.compose(reverse).decodeJson(json)       === codec.decodeJson(reverse(json))

  @Test def xmapKeys(): Unit = mapCodec.xmapKeys[String](_.reverse)(_.reverse).calc(reversed ⇒ {
    reversed.encode(Map("foo" → "bar"))         === mapCodec.encode(Map("oof" → "bar"))
    reversed.decodeJson(jsonMap("foo" → "bar")) === mapCodec.decodeJson(jsonMap("oof" → "bar"))
  })

  @Test def xmapValues(): Unit = mapCodec.xmapValues[String](_.reverse)(_.reverse).calc(reversed ⇒ {
    reversed.encode(Map("foo" → "bar"))         === mapCodec.encode(Map("foo" → "rab"))
    reversed.decodeJson(jsonMap("foo" → "bar")) === mapCodec.decodeJson(jsonMap("foo" → "rab"))
  })

  @Test def xmapDisjunction(): Unit = stringCodec.xmapDisjunction[Int](attempt(_.toInt))(_.toString).calc(intCodec ⇒ {
    intCodec.encode(3)                     === Json.jString("3")
    intCodec.decodeJson(Json.jString("3")) === DecodeResult.ok(3)
    intCodec.decodeJson(Json.jString("a")) === DecodeResult.fail("a", CursorHistory(Nil))
  })

  // Searching for a better name before making this a pimp (and one producing Either[A, B])
  private def attempt[A, B](f: A => B)(a: A): A \/ B = a.attempt(f).fold(_ => -\/(a), \/-(_))
}

class EncodeJsonTest extends JsonUtil {
  @Test def afterEncode(): Unit = encoder.afterEncode(reverse).encode(list)      === reverse(encoder.encode(list))
  @Test def andThen(): Unit     = encoder.andThen(reverse).encode(list)          === reverse(encoder.encode(list))
  @Test def downcast(): Unit    = Base.encoder.downcast[Derived].encode(derived) === derivedEncoded

  @Test def contramapKeys(): Unit =
    mapEncoder.contramapKeys[String](_.reverse).encode(Map("foo" → "bar")) === mapEncoder.encode(Map("oof" → "bar"))

  @Test def contramapValues(): Unit =
    mapEncoder.contramapValues[String](_.reverse).encode(Map("foo" → "bar")) === mapEncoder.encode(Map("foo" → "rab"))
}

class DecodeJsonTest extends JsonUtil {
  @Test def beforeDecode(): Unit = decoder.beforeDecode(reverse).decodeJson(json) === decoder.decodeJson(reverse(json))
  @Test def compose(): Unit      = decoder.compose(reverse).decodeJson(json)      === decoder.decodeJson(reverse(json))
  @Test def upcast(): Unit       = Derived.codec.upcast[Base].decodeJson(derivedEncoded) === DecodeResult.ok(derived)

  @Test def mapKeys(): Unit =
    mapDecoder.mapKeys(_.reverse).decodeJson(jsonMap("foo" → "bar")) === mapDecoder.decodeJson(jsonMap("oof" → "bar"))

  @Test def mapValues(): Unit =
    mapDecoder.mapValues(_.reverse).decodeJson(jsonMap("foo" → "bar")) === mapDecoder.decodeJson(jsonMap("foo" → "rab"))
}

class TraversalFrills extends JsonUtil {
  @Test def string(): Unit = on(jString("foo"), jNumber(3)).calling(id.string.getAll).produces(List("foo"), nil)

  private val id = Traversal.id[Json]
}

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
}
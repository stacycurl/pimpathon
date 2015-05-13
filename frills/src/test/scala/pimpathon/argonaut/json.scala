package pimpathon.argonaut

import argonaut.{CodecJson, DecodeJson, EncodeJson, Json, Parse}
import org.junit.Test

import org.junit.Assert._
import pimpathon.any._
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
}

class EncodeJsonTest extends JsonUtil {
  @Test def andThen(): Unit = assertEquals(reverse(encoder.encode(list)), encoder.andThen(reverse).encode(list))
}

class DecodeJsonTest extends JsonUtil {
  @Test def compose(): Unit = assertEquals(decoder.decodeJson(reverse(json)), decoder.compose(reverse).decodeJson(json))
}

trait JsonUtil {
  def reverse(json: Json): Json = json.withArray(_.reverse)

  val codec: CodecJson[List[String]] = CodecJson.derived[List[String]]
  val (encoder, decoder) = (codec.Encoder, codec.Decoder)
  val list = List("food", "foo", "bard", "bar")
  val json = Json.jArray(list.map(Json.jString))
}
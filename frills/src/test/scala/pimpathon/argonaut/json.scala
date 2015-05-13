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
  @Test def compose(): Unit = {
    val listCodec = CodecJson.derived[List[String]]
    val jarray = Json.jArray(List("food", "foo", "bard", "bar").map(Json.jString))

    assertEquals(listCodec.decodeJson(reverse(jarray)), listCodec.compose(reverse).decodeJson(jarray))
  }
}

class EncodeJsonTest extends JsonUtil{
  @Test def andThen(): Unit = {
    val (encodeList, list) = (implicitly[EncodeJson[List[String]]], List("food", "foo", "bard", "bar"))

    assertEquals(reverse(encodeList.encode(list)), encodeList.andThen(reverse).encode(list))
  }
}

class DecodeJsonTest extends JsonUtil{
  @Test def compose(): Unit = {
    val decodeList = implicitly[DecodeJson[List[String]]]
    val jarray = Json.jArray(List("food", "foo", "bard", "bar").map(Json.jString))

    assertEquals(decodeList.decodeJson(reverse(jarray)), decodeList.compose(reverse).decodeJson(jarray))
  }
}

trait JsonUtil {
  def reverse(json: Json): Json = json.withArray(_.reverse)
}
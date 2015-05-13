package pimpathon.argonaut

import argonaut.{EncodeJson, Json, Parse}
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

class EncodeJsonTest {
  @Test def andThen(): Unit = {
    val (encodeList, list) = (implicitly[EncodeJson[List[String]]], List("food", "foo", "bard", "bar"))

    assertEquals(reverse(encodeList.encode(list)), encodeList.andThen(reverse).encode(list))
  }

  private def reverse(json: Json): Json = json.withArray(_.reverse)
}
package pimpathon.argonautTests

import argonaut.Json._
import argonaut._
import org.junit.Test
import pimpathon.argonaut._
import sjc.delta.argonaut.json.actualExpected.flat._
import sjc.delta.argonaut.matchers._
import sjc.delta.matchers.syntax.anyDeltaMatcherOps


class JsonTest extends JsonUtil {
  @Test def filterNulls(): Unit = test(_.filterNulls,
    """null"""                        → """null""",
    """{ "a": null, "b": 3 }"""       → """{ "b": 3 }""",
    """[ "a", null, "b" ]"""          → """[ "a", "b" ]""",
    """{ "o": [ "a", null, "b" ] }""" → """{ "o": [ "a", "b" ] }""",
    """[ { "a": null, "b": 3 } ]"""   → """[ { "b": 3 } ]"""
  )

  @Test def descendant_values(): Unit = {
    jobj.descendant("age").getAll <=> List(age)
    jobj.descendant("age").modify(_ ⇒ redacted) <=> ("age" → redacted) ->: jobj

    jobj.descendant("{name, age}").getAll <=> List(name, age)
    jobj.descendant("{name, age}").modify(_ ⇒ redacted) <=> ("name" → redacted) ->: ("age" → redacted) ->: jobj

    jobj.descendant("age").int.getAll <=> List(3)
    jobj.descendant("age").int.modify(_ * 2) <=> ("age" → jNumber(6)) ->: jobj
  }

  @Test def descendant_elements(): Unit = {
    jArray(fields).descendant("[0, 2]").getAll <=> List(lying, address)

    jArray(fields).descendant("[0, 2]").modify(_ ⇒ redacted) <=> jArrayElements(
      redacted, name, redacted, age, width, preferences, potatoes, knownUnknowns, awkward
    )
  }

  @Test def descendant_all(): Unit = {
    jobj.descendant("*").getAll <=> List(name, age, lying, address, preferences, width, potatoes, knownUnknowns, awkward)

    jobj.descendant("*").modify(_ ⇒ jString("redacted")) <=> jObjectFields(
      "name" → redacted, "age" → redacted, "lying" → redacted, "address" → redacted, "preferences" → redacted,
      "width" → redacted, "potatoes" → redacted, "knownUnknowns" → redacted, "awkward" → redacted
    )
  }

  @Test def descendant_complex(): Unit = {
    jobj.descendant("preferences/*").bool.set(false)
        .descendant("address").array.string.modify("Flat B" :: _)
        .descendant("address/*").string.modify(_.toUpperCase)
        .descendant("potatoes/*/variety").string.modify(_ ⇒ "Avalanche")
        .descendant("knownUnknowns/*").int.modify(_ ⇒ 42)
        .descendant("awkward/*").string.modify(_.toUpperCase) <=> parse("""
          |{
          |  "name" : "Eric",
          |  "lying" : true,
          |  "age" : 3,
          |  "preferences" : {
          |    "bananas" : false
          |  },
          |  "address" : [
          |    "FLAT B",
          |    "29 ACACIA ROAD",
          |    "NUTTYTOWN"
          |  ],
          |  "width" : 33.5,
          |  "knownUnknowns" : {},
          |  "potatoes" : [],
          |  "awkward" : { "1": "ONE" }
          |}""".stripMargin
        )
  }

  private def test(f: Json ⇒ Json, data: (String, String)*): Unit = data.foreach {
    case (input, expected) ⇒ f(parse(input)) <=> parse(expected)
  }
}










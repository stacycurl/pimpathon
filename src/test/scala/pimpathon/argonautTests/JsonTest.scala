package pimpathon.argonautTests

import argonaut.Json._
import argonaut._
import org.junit.Test
import pimpathon.argonaut._
import pimpathon.util.on
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

  @Test def delete(): Unit = {
//    println(parse(
//      """{
//        |   "a": {
//        |     "nested": {
//        |       "thing": "bye bye"
//        |     }
//        |   },
//        |   "remainder": "still here"
//        |}
//      """.stripMargin).delete("a/nested/thing").spaces2)
//
//    println(parse("""{"candy": "lollipop", "noncandy": null,"other": "things"}""")
//      .descendant("candy").string.set("big turks").filterNulls
//      .delete("other").spaces2)
//

//    store.jsonPath("$.store.book[*].author").getAll.foreach(j ⇒ println(j.spaces2))



    val conditions = parse("""{ "conditions":
          			[
          				{ "id": "i1", "condition": true },
          				{ "id": "i2", "condition": false }
          			]
          		}""")

    Json.jArray(conditions.descendant("$.conditions[?(@['condition'] == true)].id").getAll)  <=> parse("""["i1"]""")
    Json.jArray(conditions.descendant("$.conditions[?(@['condition'] == false)].id").getAll) <=> parse("""["i2"]""")
  }

  private def print(values: List[Json]) = values.foreach(j ⇒ println(j.spaces2))


  private val store = parse(
    """
      |{
      |    "store": {
      |        "book": [
      |            {
      |                "category": "reference",
      |                "author": "Nigel Rees",
      |                "title": "Sayings of the Century",
      |                "price": 8.95
      |            },
      |            {
      |                "category": "fiction",
      |                "author": "Evelyn Waugh",
      |                "title": "Sword of Honour",
      |                "price": 12.99
      |            },
      |            {
      |                "category": "fiction",
      |                "author": "Herman Melville",
      |                "title": "Moby Dick",
      |                "isbn": "0-553-21311-3",
      |                "price": 8.99
      |            },
      |            {
      |                "category": "fiction",
      |                "author": "J. R. R. Tolkien",
      |                "title": "The Lord of the Rings",
      |                "isbn": "0-395-19395-8",
      |                "price": 22.99
      |            }
      |        ],
      |        "bicycle": {
      |            "color": "red",
      |            "price": 19.95
      |        }
      |    },
      |    "expensive": 10
      |}
    """.stripMargin)

  @Test def workWithBooleanFilters(): Unit = {
    val json = parse("""{ "conditions": [true, false, true] }""")

    json.descendant("$.conditions[?(@ == true)]").getAll  <=> List(jTrue, jTrue)
    json.descendant("$.conditions[?(@ == false)]").getAll <=> List(jFalse)
    json.descendant("$.conditions[?(false == @)]").getAll <=> List(jFalse)
  }

  @Test def `work with test set 3`(): Unit = {
    val json = parse("""{ "points": [
          				             { "id":"i1", "x": 4, "y":-5 },
          				             { "id":"i2", "x":-2, "y": 2, "z":1 },
          				             { "id":"i3", "x": 8, "y": 3 },
          				             { "id":"i4", "x":-6, "y":-1 },
          				             { "id":"i5", "x": 0, "y": 2, "z":1 },
          				             { "id":"i6", "x": 1, "y": 4 }
          				           ]
          				         }""")

    json.descendant("$.points[1]").getAll <=> List(parse("""{ "id":"i2", "x":-2, "y": 2, "z":1 }"""))
    json.descendant("$.points[4].x").getAll <=> List(jNumber(0))
    json.descendant("$.points[?(@['id']=='i4')].x").getAll <=> List(jNumber(-6))
    json.descendant("$.points[*].x").getAll <=> List(4, -2, 8, -6, 0, 1).map(jNumber)
    // Non supported syntax "$['points'][?(@['x']*@['x']+@['y']*@['y'] > 50)].id"
    json.descendant("$['points'][?(@['y'] >= 3)].id").getAll <=> List(jString("i3"), jString("i6"))
    json.descendant("$.points[?(@['z'])].id").getAll <=> List(jString("i2"), jString("i5"))
    // Non supported syntax "$.points[(count(@)-1)].id"
  }

  @Test def `"Multi-fields accessors" should "be interpreted correctly"`(): Unit = {
    val json = parse("""{"menu":{"year":2013,"file":"open","options":[{"bold":true},{"font":"helvetica"},{"size":3}]}}""")
    json.descendant("$.menu['file','year']").getAll <=> List(jNumber(2013), jString("open"))
    json.descendant("$.menu.options['foo','bar']").getAll <=> Nil
    json.descendant("$.menu.options[*]['bold','size']").getAll <=> List(jTrue, jNumber(3))
//    json.descendant("$..options['foo','bar']").getAll <=> Nil
//    json.descendant("$..options[*]['bold','size']").getAll <=> List(jTrue, jNumber(3))
  }

  @Test def workWithDeepPredicates(): Unit = {
    val json = parse(
      """{
        | "people": [
        |  {"person": {"name": "Arnie", "age": 100}, "address": "California"},
        |  {"person": {"name": "Raymond", "age": 21}, "address": "Brisvegas"}
        | ]
        |}
      """.stripMargin)
    json.descendant("$.people[?(@.person.name == 'Arnie')].address").getAll <=> List(jString("California"))
  }

  @Test def filterOnNestedFields(): Unit = {
    val json = parse(
      """[
        |  {
        |    "documentType": {
        |      "value": "Text"
        |    },
        |    "fileName": "Thing.txt"
        |  },
        |  {
        |    "documentType": {
        |      "value": "Image"
        |    },
        |    "fileName": "Duckies.img"
        |  }
        |]
      """.stripMargin)

    json.descendant("$[?(@.documentType.value == 'Text')].fileName").string.getAll <=> List("Thing.txt")
  }

  @Test def descendant_renameField(): Unit =
    parse("""{ "thing": { "original": true } }""").descendant("thing").renameField("original", "renamed") <=> parse("""{ "thing": { "renamed": true } }""")

  @Test def descendant_obj_renameField(): Unit =
    parse("""{ "thing": { "original": true } }""").descendant("thing").obj.renameField("original", "renamed") <=> parse("""{ "thing": { "renamed": true } }""")

  @Test def renameField(): Unit =
    jObjectFields("original" → jTrue).renameField("original", "renamed") <=> jObjectFields("renamed" → jTrue)

  @Test def descendant_renameFields(): Unit =
    parse("""{ "thing": { "a": true, "b": false} }""").descendant("thing").renameFields("a" → "A", "b" → "B") <=> parse("""{ "thing": { "A": true, "B": false} }""")

  @Test def descendant_obj_renameFields(): Unit =
    parse("""{ "thing": { "a": true, "b": false} }""").descendant("thing").obj.renameFields("a" → "A", "b" → "B") <=> parse("""{ "thing": { "A": true, "B": false} }""")

  @Test def renameFields(): Unit =
    jObjectFields("a" → jTrue, "b" → jFalse).renameFields("a" → "A", "b" → "B") <=> jObjectFields("A" → jTrue, "B" → jFalse)

  @Test def descendant_addIfMissing(): Unit = on(
    parse("""{ "thing": {} }"""),           parse("""{ "thing": {"a": true} }""")
  ).calling(_.descendant("thing").addIfMissing("a", jFalse)).produces(
    parse("""{ "thing": {"a": false} }"""), parse("""{ "thing": {"a": true} }""")
  )

  @Test def descendant_obj_addIfMissing(): Unit = on(
    parse("""{ "thing": {} }"""),           parse("""{ "thing": {"a": true} }""")
  ).calling(_.descendant("thing").obj.addIfMissing("a", jFalse)).produces(
    parse("""{ "thing": {"a": false} }"""), parse("""{ "thing": {"a": true} }""")
  )

  @Test def addIfMissing(): Unit = on(jEmptyObject, jObjectFields("a" → jTrue)).calling(_.addIfMissing("a", jFalse))
    .produces(jObjectFields("a" → jFalse), jObjectFields("a" → jTrue))

  private def test(f: Json ⇒ Json, data: (String, String)*): Unit = data.foreach {
    case (input, expected) ⇒ f(parse(input)) <=> parse(expected)
  }
}

class JsonObjectTest extends JsonUtil {
  @Test def renameField(): Unit =
    jObjectFields("original" → jTrue).withObject(_.renameField("original", "renamed")) <=> jObjectFields("renamed" → jTrue)

  @Test def renameFields(): Unit =
    jObjectFields("a" → jTrue, "b" → jFalse).withObject(_.renameFields("a" → "A", "b" → "B")) <=> jObjectFields("A" → jTrue, "B" → jFalse)

  @Test def addIfMissing(): Unit = on(jEmptyObject, jObjectFields("a" → jTrue))
    .calling(_.withObject(_.addIfMissing("a", jFalse))).produces(
    jObjectFields("a" → jFalse), jObjectFields("a" → jTrue)
  )
}





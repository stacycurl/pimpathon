package pimpathon.argonautTests

import argonaut.Json._
import argonaut._
import argonaut.StringWrap.StringToStringWrap
import org.junit.Test
import pimpathon.frills.any._
import pimpathon.argonaut._
import pimpathon.util.on
import pimpathon.util.AnyTestPimp
import sjc.delta.argonaut.json.actualExpected.flat._
import sjc.delta.matchers.syntax.anyDeltaMatcherOps



class JsonTest extends JsonUtil {
  @Test def indent2(): Unit = Json.obj(
    "abc" := 1,
    "def" := 2,
    "ghi" := 3,
    "jkl" := 4,
    "mno" := 5
  ).indent2 ===
    s"""{
     |  "abc" : 1,
     |  "def" : 2,
     |  "ghi" : 3,
     |  "jkl" : 4,
     |  "mno" : 5
     |}""".stripMargin

  @Test def spaces2(): Unit = Json.obj( // spaces2 isn't part of pimpathon, just comparing against indent2
    "abc" := 1,
    "def" := 2,
    "ghi" := 3,
    "jkl" := 4,
    "mno" := 5
  ).spaces2 ===
    s"""{
     |  "abc" : 1,
     |  "mno" : 5,
     |  "jkl" : 4,
     |  "def" : 2,
     |  "ghi" : 3
     |}""".stripMargin

  @Test def booleanFilter(): Unit = {
    val json = parse(
      """{
        | "people": [
        |  {"person": {"name": "Arnie", "age": 100}, "address": "California"},
        |  {"person": {"name": "Raymond", "age": 21}, "address": "Brisvegas"},
        |  {"person": {"name": "Raymond", "age": 35}, "address": "London"}
        | ]
        |}
      """.stripMargin)

    json.descendant("$.people[?(@.person.name == 'Raymond' && @.person.age == 21)].address").getAll <=> List(jString("Brisvegas"))
    json.descendant("$.people[?(@.person.name == 'Arnie' || @.person.age == 21)].address").getAll <=> List(jString("California"), jString("Brisvegas"))
  }

  @Test def filterNulls(): Unit = test(_.filterNulls,
    """null"""                        → """null""",
    """{ "a": null, "b": 3 }"""       → """{ "b": 3 }""",
    """[ "a", null, "b" ]"""          → """[ "a", "b" ]""",
    """{ "o": [ "a", null, "b" ] }""" → """{ "o": [ "a", "b" ] }""",
    """[ { "a": null, "b": 3 } ]"""   → """[ { "b": 3 } ]"""
  )

  @Test def filterR(): Unit = test(_.filterR(_ != Json.jEmptyObject),
    """{}"""                        → """null""",
    """{ "a": {}, "b": 3 }"""       → """{ "b": 3 }""",
    """[ "a", {}, "b" ]"""          → """[ "a", "b" ]""",
    """{ "o": [ "a", {}, "b" ] }""" → """{ "o": [ "a", "b" ] }""",
    """[ { "a": {}, "b": 3 } ]"""   → """[ { "b": 3 } ]"""
  )

  @Test def descendant_values(): Unit = {
    jobj.descendant("age").getAll <=> List(age)
    jobj.descendant("age").modify(_ ⇒ redacted) <=> ("age" → redacted) ->: jobj

    jobj.descendant("{name, age}").getAll <=> List(name, age)
    jobj.descendant("{name, age}").modify(_ ⇒ redacted) <=> ("name" → redacted) ->: ("age" → redacted) ->: jobj

    jobj.descendant("age").int.getAll <=> List(3)
    jobj.descendant("age").int.modify(_ * 2) <=> ("age" → jNumber(6)) ->: jobj
  }

  @Test def descendant_multiple(): Unit = {
    jobj.descendant("name", "age").getAll <=> List(name, age)
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

  @Test def descendant_ancestors(): Unit = {
    jobj.descendant("$.preferences.bananas").string.ancestors <=> jObjectFields(
      "$"                     -> Json.jArray(jobj.descendant("$").getAll),
      "$.preferences"         -> Json.jArray(jobj.descendant("$.preferences").getAll),
      "$.preferences.bananas" -> Json.jArray(jobj.descendant("$.preferences.bananas").getAll)
    )

    jobj.descendant("preferences/bananas").string.ancestors <=> jObjectFields(
      ""                    -> Json.jArray(jobj.descendant("").getAll),
      "preferences"         -> Json.jArray(jobj.descendant("preferences").getAll),
      "preferences/bananas" -> Json.jArray(jobj.descendant("preferences/bananas").getAll)
    )
  }

  @Test def descendant_firstEmptyAt(): Unit = on((path: String) ⇒ jobj.descendant(path).firstEmptyAt).maps(
    "$.preferences.bananas" → None, "$.preferences.apples" → Some("$.preferences.apples"), "$.prefs.apples" → Some("$.prefs")
  )

  @Test def as(): Unit = {
    jobj.descendant("$.address").as[Address].getAll            <=> List(Address(List("29 Acacia Road", "Nuttytown")))
    jobj.descendant("$.address").as[Address].modify(_.reverse) <=> jobj.descendant("$.address").array.modify(_.reverse)
  }

  @Test def banamanCodec(): Unit = {
    BananaMan.bananaManCodec.encode(bananaMan) <=> jobj

    BananaMan.bananaManCodec.decodeJson(jobj).toOption <=> Some(bananaMan)
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

  @Test def descendant_dynamic_complex(): Unit = {
    jobj.descendant.preferences.each.bool.set(false)
        .descendant.address.array.string.modify("Flat B" :: _)
        .descendant.address.each.string.modify(_.toUpperCase)
        .descendant.potatoes.each.variety.string.modify(_ ⇒ "Avalanche")
        .descendant.knownUnknowns.each.int.modify(_ ⇒ 42)
        .descendant.awkward.each.string.modify(_.toUpperCase) <=> parse("""
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

  @Test def descendant_complex_case_class(): Unit = {
    bananaMan
      .descendant("preferences/*").bool.set(false)
      .descendant("address").array.string.modify("Flat B" :: _)
      .descendant("address/*").string.modify(_.toUpperCase)
      .descendant("potatoes/*/variety").string.modify(_ ⇒ "Avalanche")
      .descendant("knownUnknowns/*").int.modify(_ ⇒ 42)
      .descendant("awkward/*").string.modify(_.toUpperCase) <=> BananaMan(
        name = "Eric",
        age = 3,
        lying = true,
        address = Address(List("FLAT B", "29 ACACIA ROAD", "NUTTYTOWN")),
        preferences = Preferences(bananas = false),
        width = 33.5,
        potatoes = Nil,
        knownUnknowns = KnownUnknowns(),
        awkward = Awkward(`1` = "ONE")
      )
  }

  @Test def descendant_dynamic_case_class(): Unit = {
    bananaMan
      .descendant.preferences.each.bool.set(false)
      .descendant.address.array.string.modify("Flat B" :: _)
      .descendant.address.each.string.modify(_.toUpperCase)
      .descendant.potatoes.each.variety.string.modify(_ ⇒ "Avalanche")
      .descendant.knownUnknowns.each.int.modify(_ ⇒ 42)
      .descendant.awkward.each.string.modify(_.toUpperCase) <=> BananaMan(
        name = "Eric",
        age = 3,
        lying = true,
        address = Address(List("FLAT B", "29 ACACIA ROAD", "NUTTYTOWN")),
        preferences = Preferences(bananas = false),
        width = 33.5,
        potatoes = Nil,
        knownUnknowns = KnownUnknowns(),
        awkward = Awkward(`1` = "ONE")
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

    conditions.descendant("$.conditions[?(@['condition'] == true)]").modify(_.addIfMissing("matched" := true)) <=> parse("""{
      "conditions": [
        { "id": "i1", "condition": true, "matched": true },
        { "id": "i2", "condition": false }
      ]
    }""")



    val objConditions = parse("""{ "conditions":
        {
          "first": { "id": "i1", "condition": true },
          "second": { "id": "i2", "condition": false }
        }
      }""")

    Json.jArray(objConditions.descendant("$.conditions[?(@['condition'] == true)].id").getAll)  <=> parse("""["i1"]""")
    Json.jArray(objConditions.descendant("$.conditions[?(@['condition'] == false)].id").getAll) <=> parse("""["i2"]""")

    objConditions.descendant("$.conditions[?(@['condition'] == true)]").modify(_.addIfMissing("matched" := true)) <=> parse("""{
      "conditions": {
        "first": { "id": "i1", "condition": true, "matched": true },
        "second": { "id": "i2", "condition": false }
      }
    }""")
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
    json.descendant("$.points[?(@.z)].id").getAll <=> List(jString("i2"), jString("i5"))
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

  @Test def addIfMissing(): Unit = on(
    jEmptyObject,      obj("a" := existing)
  ).calling(_.addIfMissing("a", jString(added))).produces(
    obj("a" := added), obj("a" := existing)
  )

  @Test def removeIfPresent(): Unit = on(
    jEmptyObject, obj("a" := added), obj("a" := existing)
  ).calling(_.removeIfPresent("a", jString(added))).produces(
    jEmptyObject, jEmptyObject, obj("a" := existing)
  )

  @Test def descendant_addIfMissing_many(): Unit = on(
    thing(jEmptyObject),         thing(obj("a" := existing)),
    thing(obj("b" := existing)), thing(obj("a" := existing, "b" := existing))
  ).calling(_.descendant("thing").addIfMissing("a" := added, "b" := added)).produces(
    thing(obj("a" := added, "b" := added)),    thing(obj("a" := existing, "b" := added)),
    thing(obj("a" := added, "b" := existing)), thing(obj("a" := existing, "b" := existing))
  )

  @Test def descendant_obj_addIfMissing_many(): Unit = on(
    thing(jEmptyObject),         thing(obj("a" := existing)),
    thing(obj("b" := existing)), thing(obj("a" := existing, "b" := existing))
  ).calling(_.descendant("thing").obj.addIfMissing("a" := added, "b" := added)).produces(
    thing(obj("a" := added, "b" := added)),    thing(obj("a" := existing, "b" := added)),
    thing(obj("a" := added, "b" := existing)), thing(obj("a" := existing, "b" := existing))
  )

  @Test def addIfMissing_many(): Unit = on(
    jEmptyObject,         obj("a" := existing),
    obj("b" := existing), obj("a" := existing, "b" := existing)
  ).calling(_.addIfMissing("a" := added, "b" := added)).produces(
    obj("a" := added, "b" := added),    obj("a" := existing, "b" := added),
    obj("a" := added, "b" := existing), obj("a" := existing, "b" := existing)
  )

  @Test def removeFields(): Unit = on(
    obj("a" := "value", "b" := 123, "c" := true), obj("a" := "value", "b" := 123)
  ).calling(_.removeFields("b", "c")).produces(
    obj("a" := "value"), obj("a" := "value")
  )
  
  @Test def append(): Unit = {
    val value = Json.jString("bar")
    
    Json.obj().append(Nil, value)                       <=> Json.obj()
    Json.obj().append(List("key"), value)               <=> Json.obj("key" := "bar")
    Json.obj("key" := "foo").append(List("key"), value) <=> Json.obj("key" := "bar")
    Json.obj().append(List("outer", "inner"), value)    <=> Json.obj("outer" := Json.obj("inner" := "bar"))
  }
  
  @Test def fromProperties(): Unit = {
    Json.fromProperties(Map()) <=> Right(Json.obj())
    
    Json.fromProperties(Map(
      "value" -> "\"bar\"",
      "invalid" -> "unquoted string"
    )) <=> Left(("invalid", "Unexpected content found: unquoted string"))

    Json.fromProperties(Map("foo" -> "\"bar\"")) <=> Right(Json.obj("foo" := "bar"))

    Json.fromProperties(Map("foo.oof" -> "\"bar\"")) <=> Right(Json.obj("foo" := Json.obj("oof" := "bar")))

    Json.fromProperties(Map(
      "foo.oof" -> "123",
      "foo.ffs" -> "false"
    )) <=> Right(Json.obj("foo" := Json.obj("oof" := 123, "ffs" := false)))

    Json.fromProperties(Map(
      "foo"     -> "123",
      "foo.oof" -> "false"
    )) <=> Right(Json.obj("foo" := 123))
  }

  @Test def pivot(): Unit =
    Json.obj(unpivoted.pivot: _*) <=> pivoted

  @Test def unpivot(): Unit =  
    pivoted.unpivot <=> unpivoted

  private val unpivoted: Json = Json.obj(
    "outer1" := Json.obj(
      "key1" := "value1",
      "key2" := 123,
      "inner" := Json.obj(
        "key3" := List("value3")
      )
    ),
    "outer2" := Json.obj(
      "key4" := "value4"
    )
  )
  
  private val pivoted: Json = Json.obj(
    "outer1/key1"       := "value1",
    "outer1/key2"       := 123,
    "outer1/inner/key3" := List("value3"),
    "outer2/key4"       := "value4"
  )
  

  private def test(f: Json ⇒ Json, data: (String, String)*): Unit = data.foreach {
    case (input, expected) ⇒ f(parse(input)) <=> parse(expected)
  }

  private def thing(value: Json): Json = obj("thing" → value)

  private val added    = "added"
  private val existing = "existing"
}

class JsonObjectTest extends JsonUtil {
  @Test def renameField(): Unit =
    obj("original" := true).withObject(_.renameField("original", "renamed")) <=> obj("renamed" := true)

  @Test def renameFields(): Unit =
    obj("a" := true, "b" := false).withObject(_.renameFields("a" → "A", "b" → "B")) <=> obj("A" := true, "B" := false)

  @Test def addIfMissing(): Unit = on(
    jEmptyObject,     obj("a" := existing)
  ).calling(_.withObject(_.addIfMissing("a", added))).produces(
    obj("a" := added), obj("a" := existing)
  )

  @Test def addIfMissing_many(): Unit = on(
    jEmptyObject,        obj("a" := existing),
    obj("b" := existing), obj("a" := existing, "b" := existing)
  ).calling(_.withObject(_.addIfMissing("a" := added, "b" := added))).produces(
    obj("a" := added, "b" := added),    obj("a" := existing, "b" := added),
    obj("a" := added, "b" := existing), obj("a" := existing, "b" := existing)
  )

  private val added    = jString("added")
  private val existing = jString("existing")
}





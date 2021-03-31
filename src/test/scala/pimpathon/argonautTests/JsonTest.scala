package pimpathon.argonautTests

import argonaut.Json._
import argonaut._
import argonaut.StringWrap.StringToStringWrap
import pimpathon.PSpec
import pimpathon.frills.any._
import pimpathon.argonaut._
import sjc.delta.argonaut.json.actualExpected.flat._
import sjc.delta.matchers.syntax.anyDeltaMatcherOps



class JsonSpec extends PSpec with JsonUtil {
  "indent2" in Json.obj(
    "abc" := 1,
    "def" := 2,
    "ghi" := 3,
    "jkl" := 4,
    "mno" := 5
  ).indent2 ≡
    s"""{
     |  "abc" : 1,
     |  "def" : 2,
     |  "ghi" : 3,
     |  "jkl" : 4,
     |  "mno" : 5
     |}""".stripMargin

  "spaces2" in Json.obj( // spaces2 isn't part of pimpathon, just comparing against indent2
    "abc" := 1,
    "def" := 2,
    "ghi" := 3,
    "jkl" := 4,
    "mno" := 5
  ).spaces2 ≡
    s"""{
     |  "abc" : 1,
     |  "mno" : 5,
     |  "jkl" : 4,
     |  "def" : 2,
     |  "ghi" : 3
     |}""".stripMargin

  "filterNulls" in test(_.filterNulls,
    """null"""                        → """null""",
    """{ "a": null, "b": 3 }"""       → """{ "b": 3 }""",
    """[ "a", null, "b" ]"""          → """[ "a", "b" ]""",
    """{ "o": [ "a", null, "b" ] }""" → """{ "o": [ "a", "b" ] }""",
    """[ { "a": null, "b": 3 } ]"""   → """[ { "b": 3 } ]"""
  )

  "filterR" in test(_.filterR(_ != Json.jEmptyObject),
    """{}"""                        → """null""",
    """{ "a": {}, "b": 3 }"""       → """{ "b": 3 }""",
    """[ "a", {}, "b" ]"""          → """[ "a", "b" ]""",
    """{ "o": [ "a", {}, "b" ] }""" → """{ "o": [ "a", "b" ] }""",
    """[ { "a": {}, "b": 3 } ]"""   → """[ { "b": 3 } ]"""
  )

  "banamanCodec" in {
    BananaMan.bananaManCodec.encode(bananaMan) <=> jobj

    BananaMan.bananaManCodec.decodeJson(jobj).toOption <=> Some(bananaMan)
  }

  "delete" in {
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

  "renameField" in
    jObjectFields("original" → jTrue).renameField("original", "renamed") <=> jObjectFields("renamed" → jTrue)

  "renameFields" in
    jObjectFields("a" → jTrue, "b" → jFalse).renameFields("a" → "A", "b" → "B") <=> jObjectFields("A" → jTrue, "B" → jFalse)

  "removeIfPresent" in on(
    jEmptyObject, obj("a" := added), obj("a" := existing)
  ).calling(_.removeIfPresent("a", jString(added))).produces(
    jEmptyObject, jEmptyObject, obj("a" := existing)
  )

  "removeFields" in on(
    obj("a" := "value", "b" := 123, "c" := true), obj("a" := "value", "b" := 123)
  ).calling(_.removeFields("b", "c")).produces(
    obj("a" := "value"), obj("a" := "value")
  )
  
  "append" in {
    val value = Json.jString("bar")
    
    Json.obj().append(Nil, value)                       <=> Json.obj()
    Json.obj().append(List("key"), value)               <=> Json.obj("key" := "bar")
    Json.obj("key" := "foo").append(List("key"), value) <=> Json.obj("key" := "bar")
    Json.obj().append(List("outer", "inner"), value)    <=> Json.obj("outer" := Json.obj("inner" := "bar"))
  }
  
  "fromProperties" in {
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

  "pivot" in
    Json.obj(unpivoted.pivot: _*) <=> pivoted

  "unpivot" in  
    pivoted.unpivot <=> unpivoted

  "merge" in {
    Json.obj("a" := "a", "b" := 123).merge(Json.obj("b" := 456)) <=> Json.obj("a" := "a", "b" := 456)

    Json.obj("a" := "a", "b" := 123).merge(Json.obj("b" := Json.jNull)) <=> Json.obj("a" := "a")
    
    Json.jString("non object").merge(Json.jString("anything")) <=> Json.jString("non object")
    
    Json.obj("a" := "a").merge(Json.jString("non object")) <=> Json.obj("a" := "a") 
  }
  
  "descendant" - {
    "values" in {
      jobj.descendant("age").getAll <=> List(age)
      jobj.descendant("age").modify(_ ⇒ redacted) <=> ("age" → redacted) ->: jobj
  
      jobj.descendant("{name, age}").getAll <=> List(name, age)
      jobj.descendant("{name, age}").modify(_ ⇒ redacted) <=> ("name" → redacted) ->: ("age" → redacted) ->: jobj
  
      jobj.descendant("age").int.getAll <=> List(3)
      jobj.descendant("age").int.modify(_ * 2) <=> ("age" → jNumber(6)) ->: jobj
    }

    "multiple" in {
      jobj.descendant("name", "age").getAll <=> List(name, age)
    }
  
    "elements" in {
      jArray(fields).descendant("[0, 2]").getAll <=> List(lying, address)
  
      jArray(fields).descendant("[0, 2]").modify(_ ⇒ redacted) <=> jArrayElements(
        redacted, name, redacted, age, width, preferences, potatoes, knownUnknowns, awkward
      )
    }
  
    "all" in {
      jobj.descendant("*").getAll <=> List(name, age, lying, address, preferences, width, potatoes, knownUnknowns, awkward)
  
      jobj.descendant("*").modify(_ ⇒ jString("redacted")) <=> jObjectFields(
        "name" → redacted, "age" → redacted, "lying" → redacted, "address" → redacted, "preferences" → redacted,
        "width" → redacted, "potatoes" → redacted, "knownUnknowns" → redacted, "awkward" → redacted
      )
    }
  
    "ancestors" in {
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
  
    "firstEmptyAt" in on((path: String) ⇒ jobj.descendant(path).firstEmptyAt).maps(
      "$.preferences.bananas" → None, "$.preferences.apples" → Some("$.preferences.apples"), "$.prefs.apples" → Some("$.prefs")
    )
  
    "as" in {
      jobj.descendant("$.address").as[Address].getAll            <=> List(Address(List("29 Acacia Road", "Nuttytown")))
      jobj.descendant("$.address").as[Address].modify(_.reverse) <=> jobj.descendant("$.address").array.modify(_.reverse)
    }

    "complex" in {
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

    "dynamic_complex" in {
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

    "complex_case_class" in {
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

    "dynamic_case_class" in {
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
    
    "work with test set 3" in {
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
  
    "Multi-fields accessors should be interpreted correctly" in {
      val json = parse("""{"menu":{"year":2013,"file":"open","options":[{"bold":true},{"font":"helvetica"},{"size":3}]}}""")
      json.descendant("$.menu['file','year']").getAll <=> List(jNumber(2013), jString("open"))
      json.descendant("$.menu.options['foo','bar']").getAll <=> Nil
      json.descendant("$.menu.options[*]['bold','size']").getAll <=> List(jTrue, jNumber(3))
  //    json.descendant("$..options['foo','bar']").getAll <=> Nil
  //    json.descendant("$..options[*]['bold','size']").getAll <=> List(jTrue, jNumber(3))
    }
  
    "workWithDeepPredicates" in {
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
  
    "renameField" in
      parse("""{ "thing": { "original": true } }""").descendant("thing").renameField("original", "renamed") <=> parse("""{ "thing": { "renamed": true } }""")
  
    "renameFields" in
      parse("""{ "thing": { "a": true, "b": false} }""").descendant("thing").renameFields("a" → "A", "b" → "B") <=> parse("""{ "thing": { "A": true, "B": false} }""")

    "addIfMissing" - {
      "1" in on(
        parse("""{ "thing": {} }"""),           parse("""{ "thing": {"a": true} }""")
      ).calling(_.descendant("thing").addIfMissing("a", jFalse)).produces(
        parse("""{ "thing": {"a": false} }"""), parse("""{ "thing": {"a": true} }""")
      )
    
      "many" in on(
        thing(jEmptyObject),         thing(obj("a" := existing)),
        thing(obj("b" := existing)), thing(obj("a" := existing, "b" := existing))
      ).calling(_.descendant("thing").addIfMissing("a" := added, "b" := added)).produces(
        thing(obj("a" := added, "b" := added)),    thing(obj("a" := existing, "b" := added)),
        thing(obj("a" := added, "b" := existing)), thing(obj("a" := existing, "b" := existing))
      )
    }
  
    "obj" - {
      "renameField" in
        parse("""{ "thing": { "original": true } }""").descendant("thing").obj.renameField("original", "renamed") <=> parse("""{ "thing": { "renamed": true } }""")
  
      "renameFields" in
        parse("""{ "thing": { "a": true, "b": false} }""").descendant("thing").obj.renameFields("a" → "A", "b" → "B") <=> parse("""{ "thing": { "A": true, "B": false} }""")

      "addIfMissing" - {  
        "1" in on(
          parse("""{ "thing": {} }"""),           parse("""{ "thing": {"a": true} }""")
        ).calling(_.descendant("thing").obj.addIfMissing("a", jFalse)).produces(
          parse("""{ "thing": {"a": false} }"""), parse("""{ "thing": {"a": true} }""")
        )
      
        "many" in on(
          thing(jEmptyObject),         thing(obj("a" := existing)),
          thing(obj("b" := existing)), thing(obj("a" := existing, "b" := existing))
        ).calling(_.descendant("thing").obj.addIfMissing("a" := added, "b" := added)).produces(
          thing(obj("a" := added, "b" := added)),    thing(obj("a" := existing, "b" := added)),
          thing(obj("a" := added, "b" := existing)), thing(obj("a" := existing, "b" := existing))
        )
      }
    }
    
    "booleanFilter" - {
      "1" in {
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
  
      "2" in {
        val json = parse("""{ "conditions": [true, false, true] }""")
    
        json.descendant("$.conditions[?(@ == true)]").getAll  <=> List(jTrue, jTrue)
        json.descendant("$.conditions[?(@ == false)]").getAll <=> List(jFalse)
        json.descendant("$.conditions[?(false == @)]").getAll <=> List(jFalse)
      }
    }
  }

  "addIfMissing" - {
    "1" in on(
      jEmptyObject,      obj("a" := existing)
    ).calling(_.addIfMissing("a", jString(added))).produces(
      obj("a" := added), obj("a" := existing)
    )

    "many" in on(
      jEmptyObject,         obj("a" := existing),
      obj("b" := existing), obj("a" := existing, "b" := existing)
    ).calling(_.addIfMissing("a" := added, "b" := added)).produces(
      obj("a" := added, "b" := added),    obj("a" := existing, "b" := added),
      obj("a" := added, "b" := existing), obj("a" := existing, "b" := existing)
    )
  }

  private lazy val unpivoted: Json = Json.obj(
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
  
  private lazy val pivoted: Json = Json.obj(
    "outer1/key1"       := "value1",
    "outer1/key2"       := 123,
    "outer1/inner/key3" := List("value3"),
    "outer2/key4"       := "value4"
  )

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

  private def test(f: Json ⇒ Json, data: (String, String)*): Unit = data.foreach {
    case (input, expected) ⇒ f(parse(input)) <=> parse(expected)
  }

  private def thing(value: Json): Json = obj("thing" → value)

  private lazy val added    = "added"
  private lazy val existing = "existing"
}

class JsonObjectSpec extends PSpec with JsonUtil {
  "renameField" in
    obj("original" := true).withObject(_.renameField("original", "renamed")) <=> obj("renamed" := true)

  "renameFields" in
    obj("a" := true, "b" := false).withObject(_.renameFields("a" → "A", "b" → "B")) <=> obj("A" := true, "B" := false)

  "addIfMissing" - {
    "1" in on(
      jEmptyObject,     obj("a" := existing)
    ).calling(_.withObject(_.addIfMissing("a", added))).produces(
      obj("a" := added), obj("a" := existing)
    )
  
    "many" in on(
      jEmptyObject,        obj("a" := existing),
      obj("b" := existing), obj("a" := existing, "b" := existing)
    ).calling(_.withObject(_.addIfMissing("a" := added, "b" := added))).produces(
      obj("a" := added, "b" := added),    obj("a" := existing, "b" := added),
      obj("a" := added, "b" := existing), obj("a" := existing, "b" := existing)
    )
  }

  private lazy val added    = jString("added")
  private lazy val existing = jString("existing")
}





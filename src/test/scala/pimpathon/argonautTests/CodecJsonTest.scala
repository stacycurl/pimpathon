package pimpathon.argonautTests

import _root_.argonaut._
import org.junit.Test
import pimpathon.CodecException
import pimpathon.any._
import pimpathon.argonaut._
import pimpathon.util._
import pimpathon.throwable._
import sjc.delta.argonaut.json.actualExpected.flat._
import sjc.delta.matchers.syntax.anyDeltaMatcherOps
import scalaz.{-\/, \/, \/-}


class CodecJsonTest extends JsonUtil {
  @Test def beforeDecode(): Unit = codec.beforeDecode(reverse).decodeJson(json)  === codec.decodeJson(reverse(json))
  @Test def afterDecode(): Unit  = codec.afterDecode(_.reverse).decodeJson(json) === reverse(codec.decodeJson(json))
  @Test def beforeEncode(): Unit = codec.beforeEncode(_.reverse).encode(list)    <=> codec.encode(list.reverse)
  @Test def afterEncode(): Unit  = codec.afterEncode(reverse).encode(list)       <=> reverse(codec.encode(list))
  @Test def andThen(): Unit      = codec.andThen(reverse).encode(list)           <=> reverse(codec.encode(list))
  @Test def compose(): Unit      = codec.compose(reverse).decodeJson(json)       === codec.decodeJson(reverse(json))

  @Test def xmapEntries(): Unit = mapCodec.xmapEntries[String, String](reverseEntry)(reverseEntry).calc(reversed ⇒ {
    reversed.encode(Map("foo" → "bar"))         <=> mapCodec.encode(Map("oof" → "rab"))
    reversed.decodeJson(jsonMap("foo" → "bar")) === mapCodec.decodeJson(jsonMap("oof" → "rab"))
  })

  @Test def xmapKeys(): Unit = mapCodec.xmapKeys[String](_.reverse)(_.reverse).calc(reversed ⇒ {
    reversed.encode(Map("foo" → "bar"))         <=> mapCodec.encode(Map("oof" → "bar"))
    reversed.decodeJson(jsonMap("foo" → "bar")) === mapCodec.decodeJson(jsonMap("oof" → "bar"))
  })

  @Test def xmapValues(): Unit = mapCodec.xmapValues[String](_.reverse)(_.reverse).calc(reversed ⇒ {
    reversed.encode(Map("foo" → "bar"))         <=> mapCodec.encode(Map("foo" → "rab"))
    reversed.decodeJson(jsonMap("foo" → "bar")) === mapCodec.decodeJson(jsonMap("foo" → "rab"))
  })

  @Test def xmapDisjunction(): Unit = stringCodec.xmapDisjunction[Int](attempt(_.toInt))(_.toString).calc(intCodec ⇒ {
    intCodec.encode(3)                     <=> Json.jString("3")
    intCodec.decodeJson(Json.jString("3")) === DecodeResult.ok(3)
    intCodec.decodeJson(Json.jString("a")) === DecodeResult.fail("a", CursorHistory(Nil))
  })
  
  @Test def wrapExceptions(): Unit = on(Thing(null) -> 4, (null, 3)).calling((interceptEncode _).tupled).produces(
    List(
      "pimpathon.CodecException: ",
      "\tat Encode(thingCodec).(:0)",
      "\tat Encode(hcursorCodec).(:0)",
      "Caused by: java.lang.NullPointerException"
    ),
    List(
      "pimpathon.CodecException: ",
      "\tat Encode(thingCodec).(:0)",
      "Caused by: java.lang.NullPointerException"
    )
  )

  private def interceptEncode(thing: Thing, lines: Int): List[String] =
    intercept[CodecException](CodecJson.derived[Thing].encode(thing)).stackTraceAsString().lines.toList.take(lines)
  
  private case class Thing(cursor: HCursor)

  private object Thing {
    implicit val thingCodec: CodecJson[Thing] = 
      CodecJson.derived[HCursor].wrapExceptions("hcursorCodec").xmap[Thing](Thing(_))(_.cursor).wrapExceptions("thingCodec")  
  }
  
  // Searching for a better name before making this a pimp (and one producing Either[A, B])
  private def attempt[A, B](f: A ⇒ B)(a: A): A \/ B = a.attempt(f).fold(_ => -\/(a), \/-(_))
}

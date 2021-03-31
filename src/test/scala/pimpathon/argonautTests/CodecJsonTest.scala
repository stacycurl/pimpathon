package pimpathon.argonautTests

import _root_.argonaut._
import pimpathon.any._
import pimpathon.argonaut._
import pimpathon.throwable._
import pimpathon.{CodecException, PSpec}
import scalaz.{-\/, \/, \/-}
import sjc.delta.argonaut.json.actualExpected.flat._
import sjc.delta.matchers.syntax.anyDeltaMatcherOps


class CodecJsonSpec extends PSpec with JsonUtil {
  "beforeDecode" in codec.beforeDecode(reverse).decodeJson(json)  ≡ codec.decodeJson(reverse(json))
  "afterDecode"  in codec.afterDecode(_.reverse).decodeJson(json) ≡ reverse(codec.decodeJson(json))
  "beforeEncode" in codec.beforeEncode(_.reverse).encode(list)    <=> codec.encode(list.reverse)
  "afterEncode"  in codec.afterEncode(reverse).encode(list)       <=> reverse(codec.encode(list))
  "andThen"      in codec.andThen(reverse).encode(list)           <=> reverse(codec.encode(list))
  "compose"      in codec.compose(reverse).decodeJson(json)       ≡ codec.decodeJson(reverse(json))

  "xmapEntries" in mapCodec.xmapEntries[String, String](reverseEntry)(reverseEntry).calc(reversed ⇒ {
    reversed.encode(Map("foo" → "bar"))         <=> mapCodec.encode(Map("oof" → "rab"))
    reversed.decodeJson(jsonMap("foo" → "bar")) ≡ mapCodec.decodeJson(jsonMap("oof" → "rab"))
  })

  "xmapKeys" in mapCodec.xmapKeys[String](_.reverse)(_.reverse).calc(reversed ⇒ {
    reversed.encode(Map("foo" → "bar"))         <=> mapCodec.encode(Map("oof" → "bar"))
    reversed.decodeJson(jsonMap("foo" → "bar")) ≡ mapCodec.decodeJson(jsonMap("oof" → "bar"))
  })

  "xmapValues" in mapCodec.xmapValues[String](_.reverse)(_.reverse).calc(reversed ⇒ {
    reversed.encode(Map("foo" → "bar"))         <=> mapCodec.encode(Map("foo" → "rab"))
    reversed.decodeJson(jsonMap("foo" → "bar")) ≡ mapCodec.decodeJson(jsonMap("foo" → "rab"))
  })

  "xmapDisjunction" in stringCodec.xmapDisjunction[Int](attempt(_.toInt))(_.toString).calc(intCodec ⇒ {
    intCodec.encode(3)                     <=> Json.jString("3")
    intCodec.decodeJson(Json.jString("3")) ≡ DecodeResult.ok(3)
    intCodec.decodeJson(Json.jString("a")) ≡ DecodeResult.fail("a", CursorHistory(Nil))
  })
  
  "wrapExceptions" in on(Thing(null) -> 4, (null, 3)).calling((interceptEncode _).tupled).produces(
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

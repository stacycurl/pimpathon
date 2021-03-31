package pimpathon.argonautTests

import argonaut.StringWrap.StringToStringWrap
import pimpathon.PSpec
import pimpathon.argonaut._
import sjc.delta.argonaut.json.actualExpected.flat._
import sjc.delta.matchers.syntax.anyDeltaMatcherOps


class EncodeJsonSpec extends PSpec with JsonUtil {
  "afterEncode" in encoder.afterEncode(reverse).encode(list)      <=> reverse(encoder.encode(list))
  "andThen"     in encoder.andThen(reverse).encode(list)          <=> reverse(encoder.encode(list))
  "downcast"    in Base.encoder.downcast[Derived].encode(derived) <=> derivedEncoded
  "add"         in encoder.add("length" := _.length).encode(list) <=> encoder.encode(list).addIfMissing("length" := list.length)

  "contramapEntries" in
    mapEncoder.contramapEntries[String, String](reverseEntry).encode(Map("foo" → "bar")) <=> mapEncoder.encode(Map("oof" → "rab"))

  "contramapKeys" in
    mapEncoder.contramapKeys[String](_.reverse).encode(Map("foo" → "bar")) <=> mapEncoder.encode(Map("oof" → "bar"))

  "contramapValues" in
    mapEncoder.contramapValues[String](_.reverse).encode(Map("foo" → "bar")) <=> mapEncoder.encode(Map("foo" → "rab"))
}
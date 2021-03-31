package pimpathon.argonautTests

import _root_.argonaut._
import pimpathon.PSpec
import pimpathon.argonaut._


class DecodeJsonSpec extends PSpec with JsonUtil {
  "beforeDecode" in decoder.beforeDecode(reverse).decodeJson(json) ≡ decoder.decodeJson(reverse(json))
  "compose"      in decoder.compose(reverse).decodeJson(json)      ≡ decoder.decodeJson(reverse(json))
  "upcast"       in Derived.codec.upcast[Base].decodeJson(derivedEncoded) ≡ DecodeResult.ok(derived)

  "mapEntries" in
    mapDecoder.mapEntries(reverseEntry).decodeJson(jsonMap("foo" → "bar")) ≡ mapDecoder.decodeJson(jsonMap("oof" → "rab"))

  "mapKeys" in
    mapDecoder.mapKeys(_.reverse).decodeJson(jsonMap("foo" → "bar")) ≡ mapDecoder.decodeJson(jsonMap("oof" → "bar"))

  "mapValues" in
    mapDecoder.mapValues(_.reverse).decodeJson(jsonMap("foo" → "bar")) ≡ mapDecoder.decodeJson(jsonMap("foo" → "rab"))
}

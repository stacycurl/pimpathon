package pimpathon.argonautTests

import org.junit.Test
import pimpathon.argonaut._
import sjc.delta.argonaut.json.actualExpected.flat._
import sjc.delta.argonaut.matchers._
import sjc.delta.matchers.syntax.anyDeltaMatcherOps


class EncodeJsonTest extends JsonUtil {
  @Test def afterEncode(): Unit = encoder.afterEncode(reverse).encode(list)      <=> reverse(encoder.encode(list))
  @Test def andThen(): Unit     = encoder.andThen(reverse).encode(list)          <=> reverse(encoder.encode(list))
  @Test def downcast(): Unit    = Base.encoder.downcast[Derived].encode(derived) <=> derivedEncoded

  @Test def contramapEntries(): Unit =
    mapEncoder.contramapEntries[String, String](reverseEntry).encode(Map("foo" → "bar")) <=> mapEncoder.encode(Map("oof" → "rab"))

  @Test def contramapKeys(): Unit =
    mapEncoder.contramapKeys[String](_.reverse).encode(Map("foo" → "bar")) <=> mapEncoder.encode(Map("oof" → "bar"))

  @Test def contramapValues(): Unit =
    mapEncoder.contramapValues[String](_.reverse).encode(Map("foo" → "bar")) <=> mapEncoder.encode(Map("foo" → "rab"))
}
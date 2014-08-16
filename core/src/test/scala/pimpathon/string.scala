package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.string._


class StringTest {
  @Test def prefixWith {
    assertEquals("", "".prefixWith(""))
    assertEquals("prefix", "".prefixWith("prefix"))
    assertEquals("prefix-suffix", "-suffix".prefixWith("prefix"))
    assertEquals("prefix", "prefix".prefixWith("prefix"))
  }

  @Test def sharedPrefix {
    assertEquals(("", "", ""), "".sharedPrefix(""))
    assertEquals(("1", "", ""), "1".sharedPrefix("1"))

    assertEquals(("12", "34", "43"), "1234".sharedPrefix("1243"))
  }

  @Test def suffixWith {
    assertEquals("", "".suffixWith(""))
    assertEquals("suffix", "".suffixWith("suffix"))
    assertEquals("prefix-suffix", "prefix-".suffixWith("suffix"))
    assertEquals("suffix", "suffix".suffixWith("suffix"))
  }

  @Test def prefixPadTo {
    assertEquals("ppp-suffix", "-suffix".prefixPadTo(10, 'p'))
  }
}

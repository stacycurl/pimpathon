package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.string._


class StringTest {
  @Test def prefixWith(): Unit = {
    assertEquals("", "".prefixWith(""))
    assertEquals("prefix", "".prefixWith("prefix"))
    assertEquals("prefix-suffix", "-suffix".prefixWith("prefix"))
    assertEquals("prefix", "prefix".prefixWith("prefix"))
  }

  @Test def sharedPrefix(): Unit = {
    assertEquals(("", "", ""), "".sharedPrefix(""))
    assertEquals(("1", "", ""), "1".sharedPrefix("1"))

    assertEquals(("12", "34", "43"), "1234".sharedPrefix("1243"))
  }

  @Test def suffixWith(): Unit = {
    assertEquals("", "".suffixWith(""))
    assertEquals("suffix", "".suffixWith("suffix"))
    assertEquals("prefix-suffix", "prefix-".suffixWith("suffix"))
    assertEquals("suffix", "suffix".suffixWith("suffix"))
  }

  @Test def prefixPadTo(): Unit = assertEquals("ppp-suffix", "-suffix".prefixPadTo(10, 'p'))

  @Test def md5(): Unit = assertEquals("6f1ed002ab5595859014ebf0951522d9", "blah".md5)

  @Test def emptyTo(): Unit = {
    assertEquals("abc",    "".emptyTo("abc"))
    assertEquals("def", "def".emptyTo("abc"))
  }
}
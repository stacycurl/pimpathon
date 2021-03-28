package pimpathon

import _root_.java.nio.charset.Charset

import org.junit.{Assert, Test}

import pimpathon.string._
import pimpathon.util._


class StringTest {
  @Test def stripAffixes(): Unit =
    on("(foo)", "(foo", "oof)", "ooo").calling(_.stripAffixes("(", ")")).produces("foo", "foo", "oof", "ooo")

  @Test def affixWith(): Unit =
    on("(foo)", "(foo", "oof)", "ooo").calling(_.affixWith("(", ")")).produces("(foo)", "(foo)", "(oof)", "(ooo)")

  @Test def quote(): Unit =
    on("foo", "\"foo", "foo\"", "\"foo\"").calling(_.quote).produces("\"foo\"", "\"\"foo\"", "\"foo\"\"", "\"\"foo\"\"")

  @Test def quoteWith(): Unit =
    on("foo", "'foo", "foo'", "'foo'").calling(_.quoteWith('\'')).produces("'foo'", "''foo'", "'foo''", "''foo''")

  @Test def unquote(): Unit =
    on("foo", "\"foo", "foo\"", "\"foo\"").calling(_.unquote).produces("foo", "foo", "foo", "foo")

  @Test def hyphenate(): Unit =
    on("foo", "fooFood").calling(_.hyphenate).produces("foo", "foo-food")

  @Test def prefixWith(): Unit = {
    "".prefixWith("") === ""
    on("", "-suffix", "prefix").calling(_.prefixWith("prefix")).produces("prefix", "prefix-suffix", "prefix")
  }

  @Test def sharedPrefix(): Unit = {
    "".sharedPrefix("")         === ("", "", "")
    "1".sharedPrefix("1")       === ("1", "", "")
    "1234".sharedPrefix("1243") === ("12", "34", "43")
  }

  @Test def suffixWith(): Unit = {
    "".suffixWith("") === ""
    on("", "prefix-", "suffix").calling(_.suffixWith("suffix")).produces("suffix", "prefix-suffix", "suffix")
  }

  @Test def prefixPadTo(): Unit = "-suffix".prefixPadTo(10, 'p') === "ppp-suffix"

  @Test def md5(): Unit = "blah".md5 === "6f1ed002ab5595859014ebf0951522d9"

  @Test def emptyTo(): Unit = on("", "def").calling(_.emptyTo("abc")).produces("abc", "def")

  @Test def toByteArray(): Unit = {
    Assert.assertArrayEquals(Array('a'.toByte, 'b'.toByte, 'c'.toByte), "abc".toByteArray(Charset.forName("UTF-8")))
    Assert.assertArrayEquals(Array('d'.toByte, 'e'.toByte, 'f'.toByte), "def".toByteArray)
  }

  @Test def wrap(): Unit = {
    val wrapped =
      """|Pimpathon contains pimps
         |for classes in core
         |scala & java libraries
         |and pimps for external
         |libraries""".stripMargin

    wrapped.replaceAllLiterally("\n", " ").wrap(24) === wrapped
  }
}
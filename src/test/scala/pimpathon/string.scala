package pimpathon

import _root_.java.nio.charset.Charset
import org.junit.Assert
import pimpathon.string._


class StringSpec extends PSpec {
  "stripAffixes" in
    on("(foo)", "(foo", "oof)", "ooo").calling(_.stripAffixes("(", ")")).produces("foo", "foo", "oof", "ooo")

  "affixWith" in
    on("(foo)", "(foo", "oof)", "ooo").calling(_.affixWith("(", ")")).produces("(foo)", "(foo)", "(oof)", "(ooo)")

  "quote" in
    on("foo", "\"foo", "foo\"", "\"foo\"").calling(_.quote).produces("\"foo\"", "\"\"foo\"", "\"foo\"\"", "\"\"foo\"\"")

  "quoteWith" in
    on("foo", "'foo", "foo'", "'foo'").calling(_.quoteWith('\'')).produces("'foo'", "''foo'", "'foo''", "''foo''")

  "unquote" in
    on("foo", "\"foo", "foo\"", "\"foo\"").calling(_.unquote).produces("foo", "foo", "foo", "foo")

  "hyphenate" in
    on("foo", "fooFood").calling(_.hyphenate).produces("foo", "foo-food")

  "prefixWith" in {
    "".prefixWith("") ≡ ""
    on("", "-suffix", "prefix").calling(_.prefixWith("prefix")).produces("prefix", "prefix-suffix", "prefix")
  }

  "sharedPrefix" in {
    "".sharedPrefix("")         ≡ ("", "", "")
    "1".sharedPrefix("1")       ≡ ("1", "", "")
    "1234".sharedPrefix("1243") ≡ ("12", "34", "43")
  }

  "suffixWith" in {
    "".suffixWith("") ≡ ""
    on("", "prefix-", "suffix").calling(_.suffixWith("suffix")).produces("suffix", "prefix-suffix", "suffix")
  }

  "prefixPadTo" in "-suffix".prefixPadTo(10, 'p') ≡ "ppp-suffix"

  "md5" in "blah".md5 ≡ "6f1ed002ab5595859014ebf0951522d9"

  "emptyTo" in on("", "def").calling(_.emptyTo("abc")).produces("abc", "def")

  "toByteArray" in {
    Assert.assertArrayEquals(Array('a'.toByte, 'b'.toByte, 'c'.toByte), "abc".toByteArray(Charset.forName("UTF-8")))
    Assert.assertArrayEquals(Array('d'.toByte, 'e'.toByte, 'f'.toByte), "def".toByteArray)
  }

  "wrap" in {
    val wrapped =
      """|Pimpathon contains pimps
         |for classes in core
         |scala & java libraries
         |and pimps for external
         |libraries""".stripMargin

    wrapped.replaceAllLiterally("\n", " ").wrap(24) ≡ wrapped
  }
}
package pimpathon

import _root_.java.nio.charset.Charset
import _root_.java.security.MessageDigest

import pimpathon.array._
import pimpathon.list._


object string {
  implicit class StringPimps(val self: String) extends AnyVal {
    def emptyTo(alternative: ⇒ String): String = if (self.isEmpty) alternative else self

    def quote: String = "\"" + self + "\""
    def unquote: String = self.stripAffixes("\"", "\"")

    def hyphenate: String = splitByCase("-").toLowerCase

    def stripAffixes(prefix: String, suffix: String): String = self.stripPrefix(prefix).stripSuffix(suffix)
    def affixWith(prefix: String, suffix: String): String = prefixWith(prefix).suffixWith(suffix)

    def prefixWith(prefix: String): String = if (self.startsWith(prefix)) self else prefix + self
    def suffixWith(suffix: String): String = if (self.endsWith(suffix))   self else self + suffix

    def sharedPrefix(other: String): (String, String, String) = {
      val (prefix, rest, otherRest) = self.toList.sharedPrefix(other.toList)

      (fromChars(prefix), fromChars(rest), fromChars(otherRest))
    }

    def prefixPadTo(len: Int, elem: Char): String = (elem.toString * (len - self.length)) + self

    def md5: String = MessageDigest.getInstance("MD5").digest(self.getBytes).toHex(length = 32)

    def toByteArray: Array[Byte] = self.getBytes(Charset.forName("UTF-8"))
    def toByteArray(charset: Charset): Array[Byte] = self.getBytes(charset)

    private def splitByCase(sep: String = " "): String =
      self.replaceAll("""(?<=[A-Z])(?=[A-Z][a-z])|(?<=[^A-Z])(?=[A-Z])|(?<=[A-Za-z])(?=[^A-Za-z])""", sep)
  }

  def fromChars(chars: List[Char]): String =
    (for(c ← chars) yield c)(collection.breakOut)
}
package pimpathon

import _root_.java.security.MessageDigest

import pimpathon.array._
import pimpathon.list._


object string {
  implicit class StringOps(val string: String) extends AnyVal {
    def emptyTo(alternative: => String): String = if (string.isEmpty) alternative else string

    def prefixWith(prefix: String): String = if (string.startsWith(prefix)) string else prefix + string
    def suffixWith(suffix: String): String = if (string.endsWith(suffix))   string else string + suffix

    def sharedPrefix(other: String): (String, String, String) = {
      val res@(prefix, rest, otherRest) = string.toList.sharedPrefix(other.toList)

      (fromChars(prefix), fromChars(rest), fromChars(otherRest))
    }

    def prefixPadTo(len: Int, elem: Char): String = (elem.toString * (len - string.length)) + string


    def md5: String = MessageDigest.getInstance("MD5").digest(string.getBytes).toHex(length = 32)
  }

  def fromChars(chars: List[Char]): String =
    (for(c <- chars) yield c)(collection.breakOut)
}


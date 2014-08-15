package pimpathon

import pimpathon.list._


object string {
  implicit def stringOps(string: String): StringOps = new StringOps(string)

  class StringOps(string: String) {
    def prefixWith(prefix: String): String = if (string.startsWith(prefix)) string else prefix + string
    def suffixWith(suffix: String): String = if (string.endsWith(suffix))   string else string + suffix

    def sharedPrefix(other: String): (String, String, String) = {
      val res@(prefix, rest, otherRest) = string.toList.sharedPrefix(other.toList)

      (fromChars(prefix), fromChars(rest), fromChars(otherRest))
    }
  }

  def fromChars(chars: List[Char]): String =
    (for(c <- chars) yield c)(collection.breakOut)
}


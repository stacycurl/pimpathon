package pimpathon

import pimpathon.list._


object string {
  implicit class StringOps(val string: String) extends AnyVal {
    def sharedPrefix(other: String): (String, String, String) = {
      val res@(prefix, rest, otherRest) = string.toList.sharedPrefix(other.toList)

      (fromChars(prefix), fromChars(rest), fromChars(otherRest))
    }
  }

  def fromChars(chars: List[Char]): String =
    (for(c <- chars) yield c)(collection.breakOut)
}


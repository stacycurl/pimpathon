package pimpathon.frills

import scalaz.NonEmptyList


object string {
  implicit class StringFrills(private val self: String) extends AnyVal {
    def splitToNel(by: String): NonEmptyList[String] = self.split(by).toList match {
      case head :: tail ⇒ NonEmptyList.fromSeq(head, tail)
      case _ ⇒ NonEmptyList(self)
    }
  }
}

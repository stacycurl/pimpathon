package pimpathon.scalaz.std

import _root_.scalaz.{\/, -\/, \/-}

object boolean {
  implicit class BooleanFrills(val self: Boolean) extends AnyVal {
    def disjunction[R](right: R): DisjunctionCapturer[R] = new DisjunctionCapturer[R](self, right)
  }

  class DisjunctionCapturer[R](value: Boolean, right: R) {
    def or[L](left: ⇒ L): L \/ R = if (value) \/-(right) else -\/(left)
  }
}

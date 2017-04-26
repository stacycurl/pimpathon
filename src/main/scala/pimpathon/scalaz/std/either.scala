package pimpathon.scalaz.std

import scalaz.{-\/, \/, \/-}


object either {
  implicit class EitherFrills[L, R](self: Either[L, R]) {
    def disjunction: L \/ R = self.fold(-\/(_), \/-(_))
  }
}

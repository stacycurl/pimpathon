package pimpathon.frills

import scala.util.Try
import scalaz.{-\/, \/, \/-}

import pimpathon.pimpTry._


object pimpTry {
  implicit class TryFrills[A](val value: Try[A]) extends AnyVal {
    def toDisjunction: Throwable \/ A = value.fold(-\/(_), \/-(_))
  }
}
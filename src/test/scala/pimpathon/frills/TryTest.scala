package pimpathon.frills

import pimpathon.PSpec
import pimpathon.frills.pimpTry._
import scalaz.{-\/, \/-}

import scala.util.{Failure, Success}


class TrySpec extends PSpec {
  "toDisjunction" in
    on(Success("foo"), Failure(boom)).calling(_.toDisjunction).produces(\/-("foo"), -\/(boom))
}
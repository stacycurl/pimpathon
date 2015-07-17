package pimpathon.frills

import org.junit.Test
import pimpathon.frills.pimpTry._
import pimpathon.util._

import scala.util.{Failure, Success}
import scalaz.{-\/, \/-}


class TryTest {
  @Test def toDisjunction(): Unit =
    on(Success("foo"), Failure(boom)).calling(_.toDisjunction).produces(\/-("foo"), -\/(boom))
}
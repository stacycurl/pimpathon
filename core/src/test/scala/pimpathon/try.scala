package pimpathon

import org.junit.Test
import scala.util.{Failure, Success}

import pimpathon.pimpTry._
import pimpathon.util._


class TryTest {
  @Test def getMessage(): Unit =
    on(Success("foo"), Failure(boom)).calling(_.getMessage).produces(None, Some(boom.getMessage))

  @Test def toEither(): Unit =
    on(Success("foo"), Failure(boom)).calling(_.toEither).produces(Right("foo"), Left(boom))
}
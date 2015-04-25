package pimpathon.frills

import org.junit.Test
import pimpathon.frills.any._
import pimpathon.util._

import scalaz.{Failure, Success}


class AnyTest {
  @Test def ensure(): Unit = on(1, 2).calling(_.ensure("odd")(_ % 2 == 0)).produces(Failure("odd"), Success(2))
}
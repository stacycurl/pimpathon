package pimpathon

import org.junit.Test
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

import org.junit.Assert._
import pimpathon.pimpTry._
import pimpathon.util._


class TryTest {
  @Test def toEither(): Unit = {
    assertEquals(Right[Throwable, String]("foo"), Success[String]("foo").toEither)
    assertEquals(Left[Throwable, String](boom), Failure[String](boom).toEither)
  }
}


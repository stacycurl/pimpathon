package pimpathon

import org.junit.Test
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

import org.junit.Assert._
import pimpathon.either._
import pimpathon.util._


class EitherTest {
  @Test def map {
    assertEquals(Left[String, Int]("1"), Left[Int, String](1).map(_.toString, _.length))
    assertEquals(Right[String, Int](3), Right[Int, String]("foo").map(_.toString, _.length))
  }

  @Test def tap {
    val ints    = new ListBuffer[Int]
    val strings = new ListBuffer[String]

    Left[Int, String](1).tap(ints += _, strings += _)
    assertEquals(List(1), ints.toList)
    assertEquals(Nil,     strings.toList)

    Right[Int, String]("foo").tap(ints += _, strings += _)
    assertEquals(List(1),     ints.toList)
    assertEquals(List("foo"), strings.toList)
  }

  @Test def toTry {
    assertEquals(Success[String]("foo"), Right[Throwable, String]("foo").toTry)
    assertEquals(Failure[String](boom), Left[Throwable, String](boom).toTry)
  }
}

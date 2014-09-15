package pimpathon

import org.junit.Test
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

import org.junit.Assert._
import pimpathon.either._
import pimpathon.util._


class EitherTest {
  @Test def leftOr: Unit = {
    assertEquals("left",      Left[String, String]("left").leftOr(_ + " !"))
    assertEquals("right !", Right[String, String]("right").leftOr(_ + " !"))
  }

  @Test def rightOr: Unit = {
    assertEquals("left !",  Left[String, String]("left").rightOr(_ + " !"))
    assertEquals("right", Right[String, String]("right").rightOr(_ + " !"))
  }

  @Test def bimap: Unit = {
    assertEquals(Left[String, Int]("1"), Left[Int, String](1).bimap(_.toString, _.length))
    assertEquals(Right[String, Int](3), Right[Int, String]("foo").bimap(_.toString, _.length))
  }

  @Test def leftMap: Unit = {
    assertEquals(Left[String, String]("1"), Left[Int, String](1).leftMap(_.toString))
    assertEquals(Right[String, String]("foo"), Right[Int, String]("foo").leftMap(_.toString))
  }

  @Test def rightMap: Unit = {
    assertEquals(Left[String, Int]("1"), Left[String, String]("1").rightMap(_.length))
    assertEquals(Right[Int, Int](3), Right[Int, String]("foo").rightMap(_.length))
  }

  @Test def tap: Unit = {
    val ints    = new ListBuffer[Int]
    val strings = new ListBuffer[String]

    Left[Int, String](1).tap(ints += _, strings += _)
    assertEquals(List(1), ints.toList)
    assertEquals(Nil,     strings.toList)

    Right[Int, String]("foo").tap(ints += _, strings += _)
    assertEquals(List(1),     ints.toList)
    assertEquals(List("foo"), strings.toList)
  }

  @Test def toTry: Unit = {
    assertEquals(Success[String]("foo"), Right[Throwable, String]("foo").toTry)
    assertEquals(Failure[String](boom), Left[Throwable, String](boom).toTry)
  }
}

package pimpathon

import org.junit.Test
import scala.collection.mutable.ListBuffer

import org.junit.Assert._
import pimpathon.either._


class EitherTest {
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
}

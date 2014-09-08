package pimpathon

import org.junit.Test
import scalaz.std.list._

import org.junit.Assert._
import scala.math.Numeric._
import pimpathon.numeric._


class NumericTest {
  @Test def xmap: Unit = {
    val numericString =
      implicitly[Numeric[Int]].xmap[String](_.toString, Integer.parseInt)

    assertEquals("3", numericString.plus("1", "2"))
    assertEquals("6", numericString.times("2", "3"))
    assertEquals("1", numericString.minus("3", "2"))
    assertEquals("-4", numericString.negate("4"))
    assertEquals(4, numericString.toInt("4"))
    assertEquals(4L, numericString.toLong("4"))
    assertEquals(4.0, numericString.toDouble("4"), 0.001)
    assertEquals(4.0, numericString.toFloat("4"), 0.001)
    assertEquals("4", numericString.fromInt(4))
    assertEquals(1 compare 2, numericString.compare("1", "2"))

    val numericListString =
      numericString.xmap[List[String]](List(_), _.head)

    assertEquals(List("3"), numericListString.plus(List("1"),  List("2")))
    assertEquals(List("6"), numericListString.times(List("2"), List("3")))
    assertEquals(List("1"), numericListString.minus(List("3"), List("2")))
    assertEquals(List("-4"), numericListString.negate(List("4")))
    assertEquals(4, numericListString.toInt(List("4")))
    assertEquals(4L, numericListString.toLong(List("4")))
    assertEquals(4.0, numericListString.toDouble(List("4")), 0.001)
    assertEquals(4.0, numericListString.toFloat(List("4")), 0.001)
    assertEquals(List("4"), numericListString.fromInt(4))
    assertEquals(1 compare 2, numericListString.compare(List("1"), List("2")))
  }
}

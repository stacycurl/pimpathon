package pimpathon

import org.junit.Test
import scalaz.std.list._

import org.junit.Assert._
import scala.math.Numeric._
import pimpathon.numeric._


class NumericTest {
  @Test def xmap {
    val numericString =
      implicitly[Numeric[Int]].xmap[String](_.toString, Integer.parseInt)

    assertEquals("3", numericString.plus("1", "2"))

    val numericListString =
      numericString.xmap[List[String]](List(_), _.head)

    assertEquals(List("3"), numericListString.plus(List("1"), List("2")))
  }
}

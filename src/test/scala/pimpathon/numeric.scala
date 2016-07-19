package pimpathon

import org.junit.Test

import org.junit.Assert._
import scala.math.Numeric._
import pimpathon.numeric._
import pimpathon.util._


class NumericTest {
  @Test def xmap(): Unit = {
    val numericString =
      implicitly[Numeric[Int]].xmap[String](_.toString, Integer.parseInt)

    numericString.plus("1", "2") === "3"
    numericString.times("2", "3") === "6"
    numericString.minus("3", "2") === "1"
    numericString.negate("4") === "-4"
    numericString.toInt("4") === 4
    numericString.toLong("4") === 4L
    assertEquals(4.0, numericString.toDouble("4"), 0.001)
    assertEquals(4.0, numericString.toFloat("4"), 0.001)
    numericString.fromInt(4) === "4"
    numericString.compare("1", "2") === (1 compare 2)

    val numericListString =
      numericString.xmap[List[String]](List(_), _.head)

    numericListString.plus(List("1"),  List("2")) === List("3")
    numericListString.times(List("2"), List("3")) === List("6")
    numericListString.minus(List("3"), List("2")) === List("1")
    numericListString.negate(List("4")) === List("-4")
    numericListString.toInt(List("4")) === 4
    numericListString.toLong(List("4")) === 4L
    assertEquals(4.0, numericListString.toDouble(List("4")), 0.001)
    assertEquals(4.0, numericListString.toFloat(List("4")), 0.001)
    numericListString.fromInt(4) === List("4")
    numericListString.compare(List("1"), List("2")) === (1 compare 2)
  }
}
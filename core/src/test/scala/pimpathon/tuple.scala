package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.tuple._


class TupleTest {
  @Test def calc(): Unit = {
    assertEquals("123abc", ("123", "abc").calc(_ + _))
  }

  @Test def to(): Unit = {
    implicit def intToString(i: Int): String = i.toString
    implicit def doubleToString(d: Double): String = d.toString

    assertEquals(("123", "456.0"), (123, 456.0).to[String])
  }

  @Test def tmap(): Unit = {
    assertEquals((6, "cba"), (2, "abc").tmap(_ * 3, _.reverse))
  }
}

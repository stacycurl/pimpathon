package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.tuple._


class TupleTest {
  @Test def calc {
    assertEquals("123abc", ("123", "abc").calc(_ + _))
  }

  @Test def to {
    implicit def intToString(i: Int): String = i.toString
    implicit def doubleToString(d: Double): String = d.toString

    assertEquals(("123", "456.0"), (123, 456.0).to[String])
  }
}

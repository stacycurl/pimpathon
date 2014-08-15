package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.tuple._


class TupleTest {
  @Test def calc {
    assertEquals("123abc", ("123", "abc").calc(_ + _))
  }
}

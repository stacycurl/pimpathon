package pimpathon.java.util

import org.junit.Assert._
import org.junit.Test


class CallableTest {
  @Test def create {
    assertEquals(1, callable.create(1).call())
  }
}

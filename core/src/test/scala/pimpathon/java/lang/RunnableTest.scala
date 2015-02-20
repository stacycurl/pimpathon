package pimpathon.java.lang

import org.junit.Test

import org.junit.Assert._
import pimpathon.builder._
import pimpathon.util._


class RunnableTest {
  @Test def create {
    assertEquals(List(1), ints.run(is ⇒ runnable.create(is += 1).run()))
  }
}
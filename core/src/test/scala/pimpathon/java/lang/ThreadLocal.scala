package pimpathon.java.lang

import org.junit.Test

import org.junit.Assert._


class ThreadLocalTest {
  @Test def create {
    assertEquals(1, threadLocal.create(1).get())
  }
}

package pimpathon

import org.junit.Assert._
import org.junit.Test


class ThreadLocalTest {
  @Test def create(): Unit = assertEquals(1, threadLocal.create(1).get())
}
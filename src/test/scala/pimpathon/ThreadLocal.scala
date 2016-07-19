package pimpathon

import org.junit.Test

import pimpathon.util._


class ThreadLocalTest {
  @Test def create(): Unit = threadLocal.create(1).get() === 1
}
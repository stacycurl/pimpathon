package pimpathon.java.util

import java.util.concurrent.Callable

import org.junit.Test

import org.junit.Assert._
import pimpathon.java.util.callable._


class CallableTest {
  @Test def create(): Unit = assertEquals(1, call(callable.create(1)))

  @Test def fromThunk(): Unit = assertEquals(1, call(() ⇒ 1))

  private def call[A](callable: Callable[A]): A = callable.call()
}
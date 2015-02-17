package pimpathon

import org.junit.Test
import scala.util.Random

import org.junit.Assert._
import pimpathon.random._


class RandomTest {
  @Test def between(): Unit = {
    assertTrue(new Random().between(20, 80) >= 20)
    assertTrue(new Random().between(20, 80) < 80)
  }
}
package pimpathon

import org.junit.Test
import scala.util.Random

import org.junit.Assert._
import pimpathon.random._


class RandomTest {
  @Test def between(): Unit = {
    assertTrue(Random.between(20, 80) >= 20 && Random.between(20, 80) < 80)
    assertTrue(Random.between('f', 'u') >= 'f' && Random.between('f', 'u') < 'u')
  }
}
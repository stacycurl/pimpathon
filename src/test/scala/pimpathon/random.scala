package pimpathon

import org.junit.Assert._
import pimpathon.random._

import scala.util.Random


class RandomSpec extends PSpec {
  "between" in {
    assertTrue(Random.between(20, 80) >= 20 && Random.between(20, 80) < 80)
    assertTrue(Random.between('f', 'u') >= 'f' && Random.between('f', 'u') < 'u')
  }
}
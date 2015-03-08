package pimpathon

import scala.util.Random


object random {
  implicit def randomPimps(value: Random): RandomPimps = new RandomPimps(value)

  class RandomPimps(val value: Random) {
    def between(min: Int, max: Int): Int = value.nextInt(max - min) + min
  }
}
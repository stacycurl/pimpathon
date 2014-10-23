package pimpathon

import scala.util.Random


object random {
  implicit def randomOps(value: Random): RandomOps = new RandomOps(value)

  class RandomOps(val value: Random) {
    def between(min: Int, max: Int): Int = value.nextInt(max - min) + min
  }
}

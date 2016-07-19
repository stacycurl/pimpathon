package pimpathon

import scala.util.Random


object random {
  implicit class RandomPimps(val value: Random) extends AnyVal {
    def between(min: Int, max: Int): Int = value.nextInt(max - min) + min
  }
}
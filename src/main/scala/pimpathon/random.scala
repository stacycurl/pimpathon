package pimpathon

import scala.util.Random


object random {
  implicit class RandomPimps(val self: Random) extends AnyVal {
    def between(min: Char, max: Char): Char = between(min.toInt, max.toInt).toChar
    def between(min: Int, max: Int): Int = self.nextInt(max - min) + min
  }
}
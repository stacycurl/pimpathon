package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.set._


class SetTest {
  @Test def powerSet {
    assertEquals(Set(Set.empty[Int]), Set.empty[Int].powerSet)
    assertEquals(Set(Set.empty[Int], Set(1)), Set(1).powerSet)

    assertEquals(Set(Set.empty[Int], Set(1), Set(2), Set(1, 2)), Set(1, 2).powerSet)
  }
}


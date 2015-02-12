package pimpathon.scalaz

import org.junit.Test
import scalaz.NonEmptyList

import org.junit.Assert._
import pimpathon.scalaz.nel._


class NelTests {
  @Test def distinct(): Unit = {
    assertEquals(NonEmptyList(1, 2), NonEmptyList(1, 2, 1).distinct)
  }

  @Test def distinctBy(): Unit = {
    assertEquals(NonEmptyList("foo", "bard", "foody"),
      NonEmptyList("foo", "bar", "bard", "food", "foody", "bardo").distinctBy(_.length))
  }
}

package pimpathon.scalaz

import org.junit.Test
import scalaz.NonEmptyList

import org.junit.Assert._
import pimpathon.multiMap._
import pimpathon.scalaz.nel._


class NelTests {
  @Test def distinct(): Unit = {
    assertEquals(NonEmptyList(1, 2), NonEmptyList(1, 2, 1).distinct)
  }

  @Test def distinctBy(): Unit = {
    assertEquals(NonEmptyList("foo", "bard", "foody"),
      NonEmptyList("foo", "bar", "bard", "food", "foody", "bardo").distinctBy(_.length))
  }

  @Test def asMap(): Unit = {
    assertEquals(Map(3 → "foo", 4 → "bard", 5 → "foody"),
      NonEmptyList("foo", "bard", "foody").asMap.withKeys(_.length))

    assertEquals(Map("foo" → 3, "bard" → 4, "foody" → 5),
      NonEmptyList("foo", "bard", "foody").asMap.withValues(_.length))
  }

  @Test def asMultiMap(): Unit = {
    assertEquals(Map(true → NonEmptyList("foo", "foody"), false → NonEmptyList("bard")),
      NonEmptyList("foo", "bard", "foody").asMultiMap[NonEmptyList].withKeys(_.contains("foo")))

    assertEquals(Map(1 -> NonEmptyList(1, 3)),
      NonEmptyList(0, 1, 2, 3).asMultiMap[NonEmptyList].withPFKeys { case i if i % 2 == 1 => i % 2 })
  }
}
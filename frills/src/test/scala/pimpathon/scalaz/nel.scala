package pimpathon.scalaz

import org.junit.Test
import scalaz.{Order, NonEmptyList}

import org.junit.Assert._
import pimpathon.multiMap._
import pimpathon.scalaz.nel._
import scalaz.syntax.either._


class NelTest {
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

    assertEquals(Map(1 → NonEmptyList(1, 3)),
      NonEmptyList(0, 1, 2, 3).asMultiMap[NonEmptyList].withPFKeys { case i if i % 2 == 1 ⇒ i % 2 })
  }

  @Test def max(): Unit = {
    implicit val intOrder: Order[Int] = scalaz.std.anyVal.intInstance

    assertEquals(3, NonEmptyList(1, 3, 2).max)
  }

  @Test def min(): Unit = {
    implicit val intOrder: Order[Int] = scalaz.std.anyVal.intInstance

    assertEquals(1, NonEmptyList(3, 1, 2).min)
  }

  @Test def partitionDisjunctions(): Unit = assertEquals(
    (List(1, 2), List("abc", "def")),
    NonEmptyList(1.left, "abc".right, "def".right, 2.left).partitionDisjunctions[List]
  )

  @Test def partitionEithers(): Unit = assertEquals(
    (List(1, 2), List("abc", "def")),
    NonEmptyList(Left(1), Right("abc"), Right("def"), Left(2)).partitionEithers[List]
  )

  @Test def toMultiMap(): Unit = {
    assertEquals(Map(1 → List(10, 11), 2 → List(20, 21)),
      NonEmptyList((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[List])

    assertEquals(Map(1 → NonEmptyList(10, 11), 2 → NonEmptyList(20, 21)),
      NonEmptyList((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[NonEmptyList])
  }
}
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
}
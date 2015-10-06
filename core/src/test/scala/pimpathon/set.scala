package pimpathon

import org.junit.Test
import pimpathon.util.on
import scala.collection.{mutable ⇒ M}

import org.junit.Assert._
import pimpathon.multiMap._
import pimpathon.set._


class SetTest {
  @Test def notContains(): Unit = {
    assertTrue(Set.empty[Int].notContains(3))
    assertFalse(Set(3).notContains(3))
  }

  @Test def powerSet(): Unit = {
    assertEquals(Set(Set.empty[Int]), Set.empty[Int].powerSet)
    assertEquals(Set(Set.empty[Int], Set(1)), Set(1).powerSet)

    assertEquals(Set(Set.empty[Int], Set(1), Set(2), Set(1, 2)), Set(1, 2).powerSet)
  }

  @Test def mutable(): Unit = {
    assertEquals(M.Set(1, 2), Set(1, 2).mutable)
    assertEquals(M.Set(1, 2), Set(1, 2).toMutable)
  }

  @Test def partitionEithers(): Unit = assertEquals(
    (Set(1, 2), Set("abc", "def")),
    Set(Left(1), Right("abc"), Right("def"), Left(2)).partitionEithers[Set]
  )

  @Test def toMultiMap(): Unit = on(Set((1, 10), (1, 11), (2, 20), (2, 21)))
    .calling(_.toMultiMap[List], _.toMultiMap[Set])
    .produces(Map(1 → List(10, 11), 2 → List(20, 21)), Map(1 → Set(10, 11), 2 → Set(20, 21)))

  @Test def asMultiMap_withKeys(): Unit = on(Set(0, 1, 2, 3))
    .calling(_.asMultiMap[List].withKeys(_ % 2), _.asMultiMap[Set].withKeys(_ % 2))
    .produces(Map(0 → List(0, 2), 1 → List(1, 3)), Map(0 → Set(0, 2), 1 → Set(1, 3)))
}
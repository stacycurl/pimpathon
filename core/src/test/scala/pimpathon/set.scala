package pimpathon

import org.junit.Test
import scala.collection.{mutable ⇒ M}

import org.junit.Assert._
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

  @Test def ungroupBy(): Unit = {
    def group(ci: (Char, Int)): Char = ci._1
    val original = Set('a' → 1, 'a' → 2, 'b' → 1, 'c' → 1, 'b' → 2)
    val ungrouped = original.ungroupBy(group)

    assertEquals(ungrouped.map(_.size), ungrouped.map(_.map(group).size))
    assertEquals(original, ungrouped.flatten)
  }

  @Test def partitionByPF(): Unit = assertEquals(
    (Set(2, 4), Set("one", "three")),
    Set(1, 2, 3, 4).partitionByPF(util.partial(1 → "one", 3 → "three"))
  )

  @Test def partitionEithers2(): Unit = assertEquals(
    (Set(1, 2), Set("abc", "def")),
    Set(Left(1), Right("abc"), Right("def"), Left(2)).partitionEithers[Set]
  )

  @Test def toMultiMap(): Unit = {
    assertEquals(Map(), Set.empty[(Int, Int)].toMultiMap[Set])

    assertEquals(Map(1 → Set(10, 11), 2 → Set(20, 21)),
      Set((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[Set])

    assertEquals(Map(1 → List(10, 11), 2 → List(20, 21)),
      Set((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[List])
  }
}
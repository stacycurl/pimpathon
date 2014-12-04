package pimpathon

import org.junit.Test
import scala.collection.{mutable => M}

import org.junit.Assert._
import pimpathon.set._


class SetTest {
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
    val original = Set('a' -> 1, 'a' -> 2, 'b' -> 1, 'c' -> 1, 'b' -> 2)
    val ungrouped = original.ungroupBy(group)

    assertEquals(ungrouped.map(_.size), ungrouped.map(_.map(group).size))
    assertEquals(original, ungrouped.flatten)
  }

  @Test def partitionByPF(): Unit = assertEquals(
    (Set(2, 4), Set("one", "three")),
    Set(1, 2, 3, 4).partitionByPF(util.partial(1 -> "one", 3 -> "three"))
  )
}


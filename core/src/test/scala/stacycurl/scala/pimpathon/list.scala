package stacycurl.scala.pimpathon

import org.junit.Test
import scala.collection.immutable.SortedMap
import scalaz.std.list._

import org.junit.Assert._
import scalaz.syntax.std.boolean._
import stacycurl.scala.pimpathon.list._


class ListTest {
  @Test def uncons {
    assertEquals("empty", nil[Int].uncons("empty", l => s"size: ${l.size}"))
    assertEquals("size: 3", List(1, 2, 3).uncons("empty", l => s"size: ${l.size}"))
  }

  @Test def emptyTo {
    assertEquals(List(1), nil[Int].emptyTo(List(1)))
    assertEquals(List(1, 2, 3), List(1, 2, 3).emptyTo(List(1)))
  }


  @Test def toMultiMap {
    assertEquals(Map(), List.empty[(Int, Int)].toMultiMap)
    assertEquals(Map(1 -> List(10, 11), 2 -> List(20, 21)), List((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap)
  }

  @Test def asMultiMap_withKeys {
    assertEquals(Map(), List.empty[Int].asMultiMap.withKeys(_ % 2))
    assertEquals(Map(0 -> List(0, 2), 1 -> List(1, 3)), List(0, 1, 2, 3).asMultiMap.withKeys(_ % 2))
  }

  @Test def asMultiMap_withValues {
    assertEquals(Map(), List.empty[Int].asMultiMap.withValues(_ % 2))
    assertEquals(Map(0 -> List(0), 1 -> List(1), 2 -> List(0), 3 -> List(1)), List(0, 1, 2, 3).asMultiMap.withValues(_ % 2))
  }

  @Test def asMultiMap_withSomeKeys {
    assertEquals(Map(), List.empty[Int].asMultiMap.withSomeKeys(i => (i % 2 == 1).option(i % 2)))
    assertEquals(Map(1 -> List(1, 3)), List(0, 1, 2, 3).asMultiMap.withSomeKeys(i => (i % 2 == 1).option(i % 2)))
  }

  @Test def asMultiMap_withSomeValues {
    assertEquals(Map(), List.empty[Int].asMultiMap.withSomeValues(i => (i % 2 == 1).option(i % 2)))
    assertEquals(Map(1 -> List(1), 3 -> List(1)), List(0, 1, 2, 3).asMultiMap.withSomeValues(i => (i % 2 == 1).option(i % 2)))
  }

  @Test def asMultiMap_withManyKeys {
    assertEquals(Map(), List.empty[Int].asMultiMap.withManyKeys(i => List(-i, i)))

    assertEquals(Map(1 -> List(1), 2 -> List(1, 2), 3 -> List(2)),
      List(1, 2).asMultiMap.withManyKeys(i => List(i, i + 1)))
  }


  @Test def asMap_withKeys {
    assertEquals(Map(), List.empty[Int].asMap.withKeys(_ * 2))
    assertEquals(Map(2 -> 1), List(1).asMap.withKeys(_ * 2))
  }

  @Test def asMap_withValues {
    assertEquals(Map(), List.empty[Int].asMap.withValues(_ * 2))
    assertEquals(Map(1 -> 2), List(1).asMap.withValues(_ * 2))
  }

  @Test def asMap_withSomeKeys {
    assertEquals(Map(), List.empty[Int].asMap.withSomeKeys(i => Some(i * 2)))
    assertEquals(Map(2 -> 1), List(1, 2).asMap.withSomeKeys(i => (i % 2 == 1).option(i * 2)))
  }

  @Test def asMap_withSomeValues {
    assertEquals(Map(), List.empty[Int].asMap.withSomeValues(i => Some(i * 2)))
    assertEquals(Map(1 -> 2), List(1, 2).asMap.withSomeValues(i => (i % 2 == 1).option(i * 2)))
  }

  @Test def as_SortedMap_withValues {
    assertEquals(Map(), List.empty[Int].as[SortedMap].withValues(_ * 2))
    assertEquals(Map(1 -> 2), List(1).as[SortedMap].withValues(_ * 2))
  }

  @Test def asMap_withManyKeys {
    assertEquals(Map(), List.empty[Int].asMap.withManyKeys(i => List(-i, i)))
    assertEquals(Map(-2 -> 2, -1 -> 1, 1 -> 1, 2 -> 2), List(1, 2).asMap.withManyKeys(i => List(-i, i)))
  }
}

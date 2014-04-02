package stacycurl.scala.pimpathon

import org.junit.Test
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

  @Test def toMultiMapWithKeys {
    assertEquals(Map(), List.empty[Int].toMultiMapWithKeys(_ % 2))
    assertEquals(Map(0 -> List(0, 2), 1 -> List(1, 3)), List(0, 1, 2, 3).toMultiMapWithKeys(_ % 2))
  }

  @Test def toMapWithKeys {
    assertEquals(Map(), List.empty[Int].toMapWithKeys(_ * 2))
    assertEquals(Map(2 -> 1), List(1).toMapWithKeys(_ * 2))
  }

  @Test def toMapWithValues {
    assertEquals(Map(), List.empty[Int].toMapWithValues(_ * 2))
    assertEquals(Map(1 -> 2), List(1).toMapWithValues(_ * 2))
  }

  @Test def toMapWithSomeKeys {
    assertEquals(Map(), List.empty[Int].toMapWithSomeKeys(i => Some(i * 2)))
    assertEquals(Map(2 -> 1), List(1, 2).toMapWithSomeKeys(i => (i % 2 == 1).option(i * 2)))
  }
}

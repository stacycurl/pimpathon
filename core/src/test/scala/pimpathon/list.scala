package pimpathon

import org.junit.Test
import scala.collection.immutable.SortedMap

import org.junit.Assert._
import pimpathon.list._
import pimpathon.multiMap._
import scalaz.std.list._
import scalaz.syntax.std.boolean._


class ListTest {
  @Test def uncons {
    assertEquals("empty", nil[Int].uncons("empty", l => ("size: " + l.size)))
    assertEquals("size: 3", List(1, 2, 3).uncons("empty", l => ("size: " + l.size)))
  }

  @Test def emptyTo {
    assertEquals(List(1), nil[Int].emptyTo(List(1)))
    assertEquals(List(1, 2, 3), List(1, 2, 3).emptyTo(List(1)))
  }


  @Test def toMultiMap {
    assertEquals(Map(), List.empty[(Int, Int)].toMultiMap[List])

    assertEquals(Map(1 -> List(10, 11), 2 -> List(20, 21)),
      List((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[List])

    assertEquals(Map(1 -> Set(10, 11), 2 -> Set(20, 21)),
      List((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[Set])
  }

  @Test def asMultiMap_withKeys {
    assertEquals(Map(), List.empty[Int].asMultiMap.withKeys(_ % 2))
    assertEquals(Map(0 -> List(0, 2), 1 -> List(1, 3)), List(0, 1, 2, 3).asMultiMap.withKeys(_ % 2))
    assertEquals(Map(0 -> Set(0, 2), 1 -> Set(1, 3)), List(0, 1, 2, 3).asMultiMap[Set].withKeys(_ % 2))
  }

  @Test def asMultiMap_withValues {
    assertEquals(Map(), List.empty[Int].asMultiMap.withValues(_ % 2))

    assertEquals(Map(0 -> List(0), 1 -> List(1), 2 -> List(0), 3 -> List(1)),
      List(0, 1, 2, 3).asMultiMap.withValues(_ % 2))

    assertEquals(Map(0 -> Set(0), 1 -> Set(1), 2 -> Set(0), 3 -> Set(1)),
      List(0, 1, 2, 3).asMultiMap[Set].withValues(_ % 2))
  }

  @Test def asMultiMap_withSomeKeys {
    assertEquals(Map(), List.empty[Int].asMultiMap.withSomeKeys(i => (i % 2 == 1).option(i % 2)))

    assertEquals(Map(1 -> List(1, 3)),
      List(0, 1, 2, 3).asMultiMap.withSomeKeys(i => (i % 2 == 1).option(i % 2)))
  }

  @Test def asMultiMap_withSomeValues {
    assertEquals(Map(), List.empty[Int].asMultiMap.withSomeValues(i => (i % 2 == 1).option(i % 2)))

    assertEquals(Map(1 -> List(1), 3 -> List(1)),
      List(0, 1, 2, 3).asMultiMap.withSomeValues(i => (i % 2 == 1).option(i % 2)))
  }

  @Test def asMultiMap_withPFKeys {
    assertEquals(Map(), List.empty[Int].asMultiMap.withPFKeys { case i if i % 2 == 1 => i % 2 })

    assertEquals(Map(1 -> List(1, 3)),
      List(0, 1, 2, 3).asMultiMap.withPFKeys { case i if i % 2 == 1 => i % 2 })
  }

  @Test def asMultiMap_withPFValues {
    assertEquals(Map(), List.empty[Int].asMultiMap.withPFValues { case i if i % 2 == 1 => i % 2 })

    assertEquals(Map(1 -> List(1), 3 -> List(1)),
      List(0, 1, 2, 3).asMultiMap.withPFValues { case i if i % 2 == 1 => i % 2 })
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

  @Test def asMap_withPFKeys {
    assertEquals(Map(), List.empty[Int].asMap.withPFKeys { case i => i * 2 })
    assertEquals(Map(2 -> 1), List(1, 2).asMap.withPFKeys { case i if i % 2 == 1 => i * 2 })
  }

  @Test def asMap_withPFValues {
    assertEquals(Map(), List.empty[Int].asMap.withPFValues { case i => i * 2 })
    assertEquals(Map(1 -> 2), List(1, 2).asMap.withPFValues { case i if i % 2 == 1 => i * 2 })
  }

  @Test def as_SortedMap_withValues {
    assertEquals(Map(), List.empty[Int].as[SortedMap].withValues(_ * 2))
    assertEquals(Map(1 -> 2), List(1).as[SortedMap].withValues(_ * 2))
  }

  @Test def asMap_withManyKeys {
    assertEquals(Map(), List.empty[Int].asMap.withManyKeys(i => List(-i, i)))

    assertEquals(Map(-2 -> 2, -1 -> 1, 1 -> 1, 2 -> 2),
      List(1, 2).asMap.withManyKeys(i => List(-i, i)))
  }

  @Test def attributeCounts {
    assertEquals(Map(3 -> 2, 4 -> 1), List("foo", "food", "bar").attributeCounts(_.size))
  }

  @Test def optAttributeCounts {
    import pimpathon.any._

    assertEquals(Map(3 -> 2, 4 -> 1),
      List("foo", "food", "bar", "oo").optAttributeCounts(_.size.filterSelf(_ > 2)))
  }

  @Test def collectAttributeCounts {
    assertEquals(Map(3 -> 2, 4 -> 1),
      List("foo", "food", "bar", "oo").collectAttributeCounts {
        case word if word.size > 2 => word.size
      }
    )
  }

  @Test def distinctBy {
    assertEquals(List("foo", "bard", "foody"),
      List("foo", "bar", "bard", "food", "foody", "bardo").distinctBy(_.length))
  }

  @Test def tailOption {
    assertEquals(None,          Nil.tailOption)
    assertEquals(Some(Nil),     List(0).tailOption)
    assertEquals(Some(List(1)), List(0, 1).tailOption)
  }

  @Test def const {
    assertEquals(nil[Int], nil[String].const(1))
    assertEquals(List(1, 1, 1), List('a', 'b', 'c').const(1))
  }

  @Test def sharedPrefix {
    assertEquals((Nil, Nil, Nil), nil[Int].sharedPrefix(Nil))
    assertEquals((List(1), Nil, Nil), List(1).sharedPrefix(List(1)))

    assertEquals((List(1, 2), List(3, 4), List(4, 3)), List(1, 2, 3, 4).sharedPrefix(List(1, 2, 4, 3)))
  }

  private def nil[A]: List[A] = Nil
}

package pimpathon

import org.junit.Test
import scala.collection.immutable.SortedMap

import org.junit.Assert._
import pimpathon.list._
import pimpathon.multiMap._
import pimpathon.option._
import pimpathon.util._
import scalaz.std.list._
import scalaz.syntax.std.boolean._


class ListTest {
  @Test def uncons {
    assertEquals("empty", nil[Int].uncons("empty", l => s"size: ${l.size}"))
    assertEquals("size: 3", List(1, 2, 3).uncons("empty", l => s"size: ${l.size}"))
  }

  @Test def unconsC {
    assertEquals("empty",                          nil[Int].unconsC("empty", h => t => s"head: $h, tail: $t"))
    assertEquals("head: 1, tail: List(2, 3)", List(1, 2, 3).unconsC("empty", h => t => s"head: $h, tail: $t"))
  }

  @Test def emptyTo {
    assertEquals(List(1), nil[Int].emptyTo(List(1)))
    assertEquals(List(1, 2, 3), List(1, 2, 3).emptyTo(List(1)))
  }

  @Test def mapNonEmpty {
    assertEquals(None, nil[Int].mapNonEmpty(_.reverse))
    assertEquals(Some(List(3, 2, 1)), List(1, 2, 3).mapNonEmpty(_.reverse))
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

  @Test def zipToMap {
    assertEquals(Map.empty[Int, Int], nil[Int].zipToMap(nil[Int]))
    assertEquals(Map(1 -> 2), List(1).zipToMap(List(2)))
  }

  @Test def zipWith {
    assertEquals(List(6), List(2, 0).zipWith[Int, Int](List(3))(lr => lr._1 * lr._2))
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

  @Test def countWithSize {
    assertEquals(None, nil[Int].countWithSize(_ < 1))
    assertEquals(Some((1, 1)), List(0).countWithSize(_ < 1))
    assertEquals(Some((0, 1)), List(1).countWithSize(_ < 1))
    assertEquals(Some((1, 2)), List(0, 1).countWithSize(_ < 1))
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

  @Test def headTail {
    assertException[NoSuchElementException]("headTail of empty list") {
      Nil.headTail
    }

    assertEquals((1, Nil),        List(1).headTail)
    assertEquals((1, List(2)),    List(1, 2).headTail)
    assertEquals((1, List(2, 3)), List(1, 2, 3).headTail)
  }

  @Test def initLast {
    assertException[NoSuchElementException]("initLast of empty list") {
      Nil.initLast
    }

    assertEquals((Nil, 1),        List(1).initLast)
    assertEquals((List(1), 2),    List(1, 2).initLast)
    assertEquals((List(1, 2), 3), List(1, 2, 3).initLast)
  }

  @Test def headTailOption {
    assertEquals(None,                  Nil.headTailOption)
    assertEquals(Some((1, Nil)),        List(1).headTailOption)
    assertEquals(Some((1, List(2))),    List(1, 2).headTailOption)
    assertEquals(Some((1, List(2, 3))), List(1, 2, 3).headTailOption)
  }

  @Test def initLastOption {
    assertEquals(None,                   Nil.initLastOption)
    assertEquals(Some((Nil, 1)),        List(1).initLastOption)
    assertEquals(Some((List(1), 2)),    List(1, 2).initLastOption)
    assertEquals(Some((List(1, 2), 3)), List(1, 2, 3).initLastOption)
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

  @Test def fraction {
    assertEquals(Double.NaN, nil[Int].fraction(_ => true), 0.0001)
    assertEquals(0.0, List(1).fraction(_ < 1), 0.0001)
    assertEquals(1.0, List(0).fraction(_ < 1), 0.0001)
    assertEquals(0.5, List(0, 1).fraction(_ < 1), 0.0001)
  }

  @Test def batchBy {
    assertEquals(Nil, nil[Int].batchBy(_ => true))
    assertEquals(List(List(1, 2, 3)), List(1, 2, 3).batchBy(_ => true))
  }

  @Test def seqMap {
    assertEquals(Some(Nil),      nil[Int].seqMap(_ => Some(1)))
    assertEquals(None,           List(1).seqMap(_ => None))
    assertEquals(Some(List(1)),  List(1).seqMap(Some(_)))

    assertEquals(None, List(1, 2, 3).seqMap {
      case 1 => Some(1)
      case 2 => None
      case 3 => sys.error("Shouldn't get here")
    })
  }

  @Test def prefixPadTo {
    assertEquals(List(0, 0, 0, 1, 2, 3), List(1, 2, 3).prefixPadTo(6, 0))
  }
}

package pimpathon

import org.junit.Test
import scala.collection.immutable.SortedMap
import scala.collection.{mutable => M}

import org.junit.Assert._
import pimpathon.list._
import pimpathon.option._
import pimpathon.util._
import scalaz.std.list._


class ListTest {
  @Test def uncons(): Unit = {
    assertEquals("empty", nil[Int].uncons("empty", l => s"size: ${l.size}"))
    assertEquals("size: 3", List(1, 2, 3).uncons("empty", l => s"size: ${l.size}"))
  }

  @Test def unconsC(): Unit = {
    assertEquals("empty",                          nil[Int].unconsC("empty", h => t => s"head: $h, tail: $t"))
    assertEquals("head: 1, tail: List(2, 3)", List(1, 2, 3).unconsC("empty", h => t => s"head: $h, tail: $t"))
  }

  @Test def emptyTo(): Unit = {
    assertEquals(List(1), nil[Int].emptyTo(List(1)))
    assertEquals(List(1, 2, 3), List(1, 2, 3).emptyTo(List(1)))
  }

  @Test def mapNonEmpty(): Unit = {
    assertEquals(None, nil[Int].mapNonEmpty(_.reverse))
    assertEquals(Some(List(3, 2, 1)), List(1, 2, 3).mapNonEmpty(_.reverse))
  }


  @Test def zipToMap(): Unit = {
    assertEquals(Map.empty[Int, Int], nil[Int].zipToMap(nil[Int]))
    assertEquals(Map(1 -> 2), List(1).zipToMap(List(2)))
  }

  @Test def zipWith(): Unit = {
    assertEquals(List(6), List(2, 0).zipWith[Int, Int](List(3))(lr => lr._1 * lr._2))
  }


  @Test def countWithSize(): Unit = {
    assertEquals(None, nil[Int].countWithSize(_ < 1))
    assertEquals(Some((1, 1)), List(0).countWithSize(_ < 1))
    assertEquals(Some((0, 1)), List(1).countWithSize(_ < 1))
    assertEquals(Some((1, 2)), List(0, 1).countWithSize(_ < 1))
  }

  @Test def sizeGT(): Unit = {
    assertTrue(nil[Int].sizeGT(-1))
    assertFalse(nil[Int].sizeGT(0))
    assertTrue(List(1, 2).sizeGT(1))
    assertFalse(List(1, 2).sizeGT(2))
  }


  @Test def distinctBy(): Unit = {
    assertEquals(List("foo", "bard", "foody"),
      List("foo", "bar", "bard", "food", "foody", "bardo").distinctBy(_.length))
  }

  @Test def tailOption(): Unit = {
    assertEquals(None,          Nil.tailOption)
    assertEquals(Some(Nil),     List(0).tailOption)
    assertEquals(Some(List(1)), List(0, 1).tailOption)
  }

  @Test def headTail(): Unit = {
    assertException[NoSuchElementException]("headTail of empty list") {
      Nil.headTail
    }

    assertEquals((1, Nil),        List(1).headTail)
    assertEquals((1, List(2)),    List(1, 2).headTail)
    assertEquals((1, List(2, 3)), List(1, 2, 3).headTail)
  }

  @Test def initLast(): Unit = {
    assertException[NoSuchElementException]("initLast of empty list") {
      Nil.initLast
    }

    assertEquals((Nil, 1),        List(1).initLast)
    assertEquals((List(1), 2),    List(1, 2).initLast)
    assertEquals((List(1, 2), 3), List(1, 2, 3).initLast)
  }

  @Test def headTailOption(): Unit = {
    assertEquals(None,                  Nil.headTailOption)
    assertEquals(Some((1, Nil)),        List(1).headTailOption)
    assertEquals(Some((1, List(2))),    List(1, 2).headTailOption)
    assertEquals(Some((1, List(2, 3))), List(1, 2, 3).headTailOption)
  }

  @Test def initLastOption(): Unit = {
    assertEquals(None,                   Nil.initLastOption)
    assertEquals(Some((Nil, 1)),        List(1).initLastOption)
    assertEquals(Some((List(1), 2)),    List(1, 2).initLastOption)
    assertEquals(Some((List(1, 2), 3)), List(1, 2, 3).initLastOption)
  }

  @Test def const(): Unit = {
    assertEquals(nil[Int], nil[String].const(1))
    assertEquals(List(1, 1, 1), List('a', 'b', 'c').const(1))
  }

  @Test def sharedPrefix(): Unit = {
    assertEquals((Nil, Nil, Nil), nil[Int].sharedPrefix(Nil))
    assertEquals((List(1), Nil, Nil), List(1).sharedPrefix(List(1)))

    assertEquals((List(1, 2), List(3, 4), List(4, 3)), List(1, 2, 3, 4).sharedPrefix(List(1, 2, 4, 3)))
  }

  @Test def fraction(): Unit = {
    assertEquals(Double.NaN, nil[Int].fraction(_ => true), 0.0001)
    assertEquals(0.0, List(1).fraction(_ < 1), 0.0001)
    assertEquals(1.0, List(0).fraction(_ < 1), 0.0001)
    assertEquals(0.5, List(0, 1).fraction(_ < 1), 0.0001)
  }

  @Test def batchBy(): Unit = {
    assertEquals(Nil, nil[Int].batchBy(_ => true))
    assertEquals(List(List(1, 2, 3)), List(1, 2, 3).batchBy(_ => true))
  }

  @Test def seqMap(): Unit = {
    assertEquals(Some(Nil),      nil[Int].seqMap(_ => Some(1)))
    assertEquals(None,           List(1).seqMap(_ => None))
    assertEquals(Some(List(1)),  List(1).seqMap(Some(_)))

    assertEquals(None, List(1, 2, 3).seqMap {
      case 1 => Some(1)
      case 2 => None
      case 3 => goBoom
    })
  }

  @Test def prefixPadTo(): Unit = {
    assertEquals(List(0, 0, 0, 1, 2, 3), List(1, 2, 3).prefixPadTo(6, 0))
  }

  @Test def ungroupBy(): Unit = {
    assertEquals(List(
      List('a' -> 1, 'b' -> 1, 'c' -> 1),
      List('a' -> 2, 'b' -> 2)
    ), List('a' -> 1, 'a' -> 2, 'b' -> 1, 'c' -> 1, 'b' -> 2).ungroupBy(_._1))
  }

  @Test def tap(): Unit = {
    val strings = new M.ListBuffer[String]

    nil[Int].tap(strings += "empty", _ => strings += "non-empty")
    assertEquals(List("empty"), strings.toList)

    List(1).tap(strings += "empty", _ => strings += "non-empty")
    assertEquals(List("empty", "non-empty"), strings.toList)
  }

  @Test def tapEmpty(): Unit = {
    val strings = new M.ListBuffer[String]

    List(1).tapEmpty(strings += "empty")
    assertEquals(nil[String], strings.toList)

    nil[Int].tapEmpty(strings += "empty")
    assertEquals(List("empty"), strings.toList)
  }

  @Test def tapNonEmpty(): Unit = {
    val strings = new M.ListBuffer[String]

    nil[Int].tapNonEmpty(_ => strings += "non-empty")
    assertEquals(nil[String], strings.toList)

    List(1).tapNonEmpty(_ => strings += "non-empty")
    assertEquals(List("non-empty"), strings.toList)
  }

  @Test def amass(): Unit = assertEquals(
    List(2, -2, 4, -4),
    List(1, 2, 3, 4).amass { case i if i % 2 == 0 => List(i, -i) }
  )

  @Test def partitionEithers(): Unit = assertEquals(
    (List(1, 2), List("abc", "def")),
    List(Left(1), Right("abc"), Right("def"), Left(2)).partitionEithers
  )

  @Test def partitionByPF(): Unit = assertEquals(
    (List(2, 4), List("one", "three")),
    List(1, 2, 3, 4).partitionByPF(util.partial(1 -> "one", 3 -> "three"))
  )
}

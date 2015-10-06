package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.builder._
import pimpathon.list._
import pimpathon.util._


class ListTest {
  @Test def uncons(): Unit = on(nil[Int], List(1, 2, 3))
    .calling(_.uncons("empty", l ⇒ s"size: ${l.size}")).produces("empty", "size: 3")

  @Test def unconsC(): Unit = on(nil[Int], List(1, 2, 3))
    .calling(_.unconsC("empty", h ⇒ t ⇒ s"head: $h, tail: $t")).produces("empty", "head: 1, tail: List(2, 3)")

  @Test def unsnocC(): Unit = on(nil[Int], List(1, 2, 3))
    .calling(_.unsnocC("empty", i ⇒ l ⇒ s"init: $i, last: $l")).produces("empty", "init: List(1, 2), last: 3")

  @Test def emptyTo(): Unit = on(nil[Int], List(1, 2, 3)).calling(_.emptyTo(List(1))).produces(List(1), List(1, 2, 3))

  @Test def calcIfNonEmpty(): Unit = on(nil[Int], List(1, 2, 3))
    .calling(_.calcIfNonEmpty(_.reverse)).produces(None, Some(List(3, 2, 1)))

  @Test def mapIfNonEmpty(): Unit = on(nil[Int], List(1, 2, 3))
    .calling(_.mapIfNonEmpty(_ * 2)).produces(None, Some(List(2, 4, 6)))

  @Test def zipToMap(): Unit = {
    assertEquals(Map.empty[Int, Int], nil[Int].zipToMap(nil[Int]))
    assertEquals(Map(1 → 2), List(1).zipToMap(List(2)))
  }

  @Test def zipWith(): Unit = assertEquals(List(6), List(2, 0).zipWith[Int, Int](List(3))(lr ⇒ lr._1 * lr._2))


  @Test def countWithSize(): Unit = on(nil[Int], List(0), List(1), List(0, 1))
    .calling(_.countWithSize(_ < 1)).produces(None, Some((1, 1)), Some((0, 1)), Some((1, 2)))

  @Test def sizeGT(): Unit = {
    assertTrue(nil[Int].sizeGT(-1))
    assertFalse(nil[Int].sizeGT(0))
    assertTrue(List(1, 2).sizeGT(1))
    assertFalse(List(1, 2).sizeGT(2))
  }

  @Test def duplicates(): Unit = assertEquals(
    List("bar", "bar", "foo", "foo", "foo"), List("foo", "bar", "foo", "food", "bar", "foo").duplicates
  )

  @Test def duplicatesBy(): Unit = assertEquals(
    List("bard", "food", "foo", "bar"), List("foo", "bar", "bard", "food", "foody").duplicatesBy(_.length)
  )

  @Test def distinctBy(): Unit = {
    assertEquals(List("foo", "bard", "foody"),
      List("foo", "bar", "bard", "food", "foody", "bardo").distinctBy(_.length))
  }

  @Test def countBy(): Unit = assertEquals(
    Map(1 → List("foo"), 2 → List("foody", "barby"), 3 → List("bard", "food", "barb")),
    List("foo", "bard", "food", "barb", "foody", "barby").countBy(_.length)
  )

  @Test def tailOption(): Unit =
    on(nil[Int], List(0), List(0, 1)).calling(_.tailOption).produces(None, Some(Nil), Some(List(1)))

  @Test def headTail(): Unit = {
    on(List(1), List(1, 2), List(1, 2, 3)).calling(_.headTail).produces((1, Nil), (1, List(2)), (1, List(2, 3)))
    assertThrows[NoSuchElementException]("headTail of empty list")(Nil.headTail)
  }

  @Test def initLast(): Unit = {
    on(List(1), List(1, 2), List(1, 2, 3)).calling(_.initLast).produces((Nil, 1), (List(1), 2), (List(1, 2), 3))
    assertThrows[NoSuchElementException]("initLast of empty list")(Nil.initLast)
  }

  @Test def headTailOption(): Unit = on(nil[Int], List(1), List(1, 2), List(1, 2, 3))
    .calling(_.headTailOption).produces(None, Some((1, Nil)), Some((1, List(2))), Some((1, List(2, 3))))

  @Test def initLastOption(): Unit = on(nil[Int], List(1), List(1, 2), List(1, 2, 3))
    .calling(_.initLastOption).produces(None, Some((Nil, 1)), Some((List(1), 2)), Some((List(1, 2), 3)))

  @Test def initOption(): Unit = on(nil[Int], List(1), List(1, 2), List(1, 2, 3))
    .calling(_.initOption).produces(None, Some(Nil), Some(List(1)), Some(List(1, 2)))

  @Test def const(): Unit = on(nil[Int], List('a', 'b', 'c')).calling(_.const(1)).produces(nil[Int], List(1, 1, 1))

  @Test def sharedPrefix(): Unit = {
    assertEquals((Nil, Nil, Nil), nil[Int].sharedPrefix(Nil))
    assertEquals((List(1), Nil, Nil), List(1).sharedPrefix(List(1)))

    assertEquals((List(1, 2), List(3, 4), List(4, 3)), List(1, 2, 3, 4).sharedPrefix(List(1, 2, 4, 3)))
  }

  @Test def fraction(): Unit = {
    assertEquals(Double.NaN, nil[Int].fraction(_ ⇒ true), 0.0001)
    assertEquals(0.0, List(1).fraction(_ < 1), 0.0001)
    assertEquals(1.0, List(0).fraction(_ < 1), 0.0001)
    assertEquals(0.5, List(0, 1).fraction(_ < 1), 0.0001)
  }

  @Test def batchBy(): Unit = {
    assertEquals(Nil, nil[Int].batchBy(_ ⇒ true))

    assertEquals(
      List(List(1 → 1, 1 → 2), List(2 → 1), List(1 → 3), List(2 → 2, 2 → 3)),
      List(     1 → 1, 1 → 2,       2 → 1,       1 → 3,       2 → 2, 2 → 3).batchBy(_._1)
    )
  }

  @Test def prefixPadTo(): Unit = {
    assertEquals(List(0, 0, 0, 1, 2, 3), List(1, 2, 3).prefixPadTo(6, 0))
  }

  @Test def tap(): Unit = {
    assertEquals(List("empty"),     strings().run(ss ⇒ nil[Int].tap(ss += "empty", _ ⇒ ss += "non-empty")))
    assertEquals(List("non-empty"), strings().run(ss ⇒  List(1).tap(ss += "empty", _ ⇒ ss += "non-empty")))
  }

  @Test def tapEmpty(): Unit = {
    assertEquals(List("empty"), strings().run(ss ⇒ nil[Int].tapEmpty(ss += "empty")))
    assertEquals(Nil,           strings().run(ss ⇒  List(1).tapEmpty(ss += "empty")))
  }

  @Test def tapNonEmpty(): Unit = {
    assertEquals(Nil,               strings().run(ss ⇒ nil[Int].tapNonEmpty(_ ⇒ ss += "non-empty")))
    assertEquals(List("non-empty"), strings().run(ss ⇒  List(1).tapNonEmpty(_ ⇒ ss += "non-empty")))
  }

  @Test def amass(): Unit = assertEquals(
    List(2, -2, 4, -4),
    List(1, 2, 3, 4).amass { case i if i % 2 == 0 ⇒ List(i, -i) }
  )

  @Test def cartesianProduct(): Unit = assertEquals(
    for { a ← List(1, 2); b ← List(10, 20); c ← List(100, 200) } yield List(a, b, c),
    List(List(1, 2), List(10, 20), List(100, 200)).cartesianProduct
  )

  @Test def onlyOrThrow(): Unit = {
    on(nil[Int], List(1, 2)).calling(_.onlyOrThrow(exception)).throws("List()", "List(1, 2)")
    assertEquals(1, List(1).onlyOrThrow(_ ⇒ new Exception()))
  }

  @Test def onlyEither(): Unit =
    on(nil[Int], List(1, 2), List(1)).calling(_.onlyEither).produces(Left(Nil), Left(List(1, 2)), Right(1))

  @Test def onlyOption(): Unit = on(nil[Int], List(1, 2), List(1)).calling(_.onlyOption).produces(None, None, Some(1))

  @Test def zipExact(): Unit = {
    assertEquals((Nil, None),                             Nil.zipExact(Nil))
    assertEquals((List((1, 4), (2, 5), (3, 6)), None),    List(1, 2, 3).zipExact(List(4, 5, 6)))
    assertEquals((Nil, Some(Left(List(1, 2, 3)))),        List(1, 2, 3).zipExact(Nil))
    assertEquals((Nil, Some(Right(List(4, 5, 6)))),       Nil.zipExact(List(4, 5, 6)))
    assertEquals((List((1, 4)), Some(Left(List(2, 3)))),  List(1, 2, 3).zipExact(List(4)))
    assertEquals((List((1, 4)), Some(Right(List(5, 6)))), List(1).zipExact(List(4, 5, 6)))
  }

  @Test def zipExactWith(): Unit = {
    assertEquals((nil[Int], None),                       nil[Int].zipExactWith(nil[Int])(_ + _))
    assertEquals((List(5, 7, 9), None),                  List(1, 2, 3).zipExactWith(List(4, 5, 6))(_ + _))
    assertEquals((nil[Int], Some(Left(List(1, 2, 3)))),  List(1, 2, 3).zipExactWith(nil[Int])(_ + _))
    assertEquals((nil[Int], Some(Right(List(4, 5, 6)))), nil[Int].zipExactWith(List(4, 5, 6))(_ + _))
    assertEquals((List(5), Some(Left(List(2, 3)))),      List(1, 2, 3).zipExactWith(List(4))(_ + _))
    assertEquals((List(5), Some(Right(List(5, 6)))),     List(1).zipExactWith(List(4, 5, 6))(_ + _))
  }

  @Test def partitionEithers(): Unit = assertEquals(
    (List(1, 2), List("abc", "def")),
    List(Left(1), Right("abc"), Right("def"), Left(2)).partitionEithers[List]
  )

  @Test def toMultiMap(): Unit = on(List((1, 10), (1, 11), (2, 20), (2, 21)))
    .calling(_.toMultiMap[List], _.toMultiMap[Set])
    .produces(Map(1 → List(10, 11), 2 → List(20, 21)), Map(1 → Set(10, 11), 2 → Set(20, 21)))
}

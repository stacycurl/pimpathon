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
    nil[Int].zipToMap(nil[Int]) === Map.empty[Int, Int]
    List(1).zipToMap(List(2))   === Map(1 → 2)
  }

  @Test def zipWith(): Unit = List(2, 0).zipWith[Int, Int](List(3))(lr ⇒ lr._1 * lr._2) === List(6)


  @Test def countWithSize(): Unit = on(nil[Int], List(0), List(1), List(0, 1))
    .calling(_.countWithSize(_ < 1)).produces(None, Some((1, 1)), Some((0, 1)), Some((1, 2)))

  @Test def sizeGT(): Unit = {
    assertTrue(nil[Int].sizeGT(-1))
    assertFalse(nil[Int].sizeGT(0))
    assertTrue(List(1, 2).sizeGT(1))
    assertFalse(List(1, 2).sizeGT(2))
  }

  @Test def duplicates(): Unit =
    List("foo", "bar", "foo", "food", "bar", "foo").duplicates === List("bar", "bar", "foo", "foo", "foo")

  @Test def duplicatesBy(): Unit =
    List("foo", "bar", "bard", "food", "foody").duplicatesBy(_.length) === List("bard", "food", "foo", "bar")

  @Test def distinctBy(): Unit =
    List("foo", "bar", "bard", "food", "foody", "bardo").distinctBy(_.length) === List("foo", "bard", "foody")

  @Test def countBy(): Unit = List("foo", "bard", "food", "barb", "foody", "barby").countBy(_.length) ===
    Map(1 → List("foo"), 2 → List("foody", "barby"), 3 → List("bard", "food", "barb"))

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

  @Test def mapC(): Unit =
    on(nil[(Int, Int)], List((1, 2), (2, 3))).calling(_.mapC(a ⇒ b ⇒ a * b)).produces(nil[Int], List(2, 6))

  @Test def lpair(): Unit =
    on(nil[Int], List(1, 2, 3)).calling(_.lpair(_ * 2)).produces(nil[(Int, Int)], List((2,1), (4,2), (6,3)))

  @Test def rpair(): Unit =
    on(nil[Int], List(1, 2, 3)).calling(_.rpair(_ * 2)).produces(nil[(Int, Int)], List((1,2), (2,4), (3,6)))

  @Test def sharedPrefix(): Unit = {
    nil[Int].sharedPrefix(Nil)                      === (Nil, Nil, Nil)
    List(1).sharedPrefix(List(1))                   === (List(1), Nil, Nil)
    List(1, 2, 3, 4).sharedPrefix(List(1, 2, 4, 3)) === (List(1, 2), List(3, 4), List(4, 3))
  }

  @Test def fraction(): Unit = {
    assertEquals(Double.NaN, nil[Int].fraction(_ ⇒ true), 0.0001)
    assertEquals(0.0, List(1).fraction(_ < 1), 0.0001)
    assertEquals(1.0, List(0).fraction(_ < 1), 0.0001)
    assertEquals(0.5, List(0, 1).fraction(_ < 1), 0.0001)
  }

  @Test def batchBy(): Unit = {
    nil[Int].batchBy(_ ⇒ true) === Nil

         List(1 → 1, 1 → 2,       2 → 1,       1 → 3,       2 → 2, 2 → 3).batchBy(_._1) ===
    List(List(1 → 1, 1 → 2), List(2 → 1), List(1 → 3), List(2 → 2, 2 → 3))
  }

  @Test def prefixPadTo(): Unit = List(1, 2, 3).prefixPadTo(6, 0) === List(0, 0, 0, 1, 2, 3)

  @Test def tap(): Unit = {
    strings().run(ss ⇒ nil[Int].tap(ss += "empty", _ ⇒ ss += "non-empty")) === List("empty")
    strings().run(ss ⇒  List(1).tap(ss += "empty", _ ⇒ ss += "non-empty")) === List("non-empty")
  }

  @Test def tapEmpty(): Unit = {
    strings().run(ss ⇒ nil[Int].tapEmpty(ss += "empty")) === List("empty")
    strings().run(ss ⇒  List(1).tapEmpty(ss += "empty")) === Nil
  }

  @Test def tapNonEmpty(): Unit = {
    strings().run(ss ⇒ nil[Int].tapNonEmpty(_ ⇒ ss += "non-empty")) === Nil
    strings().run(ss ⇒  List(1).tapNonEmpty(_ ⇒ ss += "non-empty")) === List("non-empty")
  }

  @Test def amass(): Unit = List(1, 2, 3, 4).amass { case i if i % 2 == 0 ⇒ List(i, -i) } === List(2, -2, 4, -4)

  @Test def cartesianProduct(): Unit = List(List(1, 2), List(10, 20), List(100, 200)).cartesianProduct ===
    (for { a ← List(1, 2); b ← List(10, 20); c ← List(100, 200) } yield List(a, b, c))

  @Test def onlyOrThrow(): Unit = {
    on(nil[Int], List(1, 2)).calling(_.onlyOrThrow(exception)).throws("List()", "List(1, 2)")
    List(1).onlyOrThrow(_ ⇒ new Exception()) === 1
  }

  @Test def onlyEither(): Unit =
    on(nil[Int], List(1, 2), List(1)).calling(_.onlyEither).produces(Left(Nil), Left(List(1, 2)), Right(1))

  @Test def onlyOrEither(): Unit =
    on(nil[Int], List(1, 2), List(1)).calling(_.onlyOrEither(_.size)).produces(Left(0), Left(2), Right(1))

  @Test def onlyOption(): Unit = on(nil[Int], List(1, 2), List(1)).calling(_.onlyOption).produces(None, None, Some(1))

  @Test def zipExact(): Unit = {
    Nil.zipExact(Nil)                     === (Nil, None)
    List(1, 2, 3).zipExact(List(4, 5, 6)) === (List((1, 4), (2, 5), (3, 6)), None)
    List(1, 2, 3).zipExact(Nil)           === (Nil, Some(Left(List(1, 2, 3))))
    Nil.zipExact(List(4, 5, 6))           === (Nil, Some(Right(List(4, 5, 6))))
    List(1, 2, 3).zipExact(List(4))       === (List((1, 4)), Some(Left(List(2, 3))))
    List(1).zipExact(List(4, 5, 6))       === (List((1, 4)), Some(Right(List(5, 6))))
  }

  @Test def zipExactWith(): Unit = {
    nil[Int].zipExactWith(nil[Int])(_ + _)           === (nil[Int], None)
    List(1, 2, 3).zipExactWith(List(4, 5, 6))(_ + _) === (List(5, 7, 9), None)
    List(1, 2, 3).zipExactWith(nil[Int])(_ + _)      === (nil[Int], Some(Left(List(1, 2, 3))))
    nil[Int].zipExactWith(List(4, 5, 6))(_ + _)      === (nil[Int], Some(Right(List(4, 5, 6))))
    List(1, 2, 3).zipExactWith(List(4))(_ + _)       === (List(5), Some(Left(List(2, 3))))
    List(1).zipExactWith(List(4, 5, 6))(_ + _)       === (List(5), Some(Right(List(5, 6))))
  }

  @Test def partitionEithers(): Unit =
    List(Left(1), Right("abc"), Right("def"), Left(2)).partitionEithers[List] === (List(1, 2), List("abc", "def"))

  @Test def toMultiMap(): Unit = on(List((1, 10), (1, 11), (2, 20), (2, 21)))
    .calling(_.toMultiMap[List], _.toMultiMap[Set])
    .produces(Map(1 → List(10, 11), 2 → List(20, 21)), Map(1 → Set(10, 11), 2 → Set(20, 21)))

  @Test def sortPromoting(): Unit =
    List("red", "green", "black", "purple").sortPromoting("purple", "green") === List("purple", "green", "black", "red")

  @Test def sortDemoting(): Unit =
    List("black", "red", "purple", "green").sortDemoting("green", "black") === List("purple", "red", "green", "black")

  @Test def shuffle(): Unit = List.range(1, 10).shuffle().sorted === List.range(1, 10)
}

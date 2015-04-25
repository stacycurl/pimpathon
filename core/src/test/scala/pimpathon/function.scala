package pimpathon

import org.junit.Test
import scala.util.Random

import org.junit.Assert._
import pimpathon.function._
import pimpathon.util._


class FunctionTest {
  @Test def guardWith(): Unit =
    on(1, 2, 3, 4).calling((double guardWith isEven).lift).produces(None, Some(4), None, Some(8))

  private val isEven: Predicate[Int] = _ % 2 == 0
  private val double: (Int ⇒ Int)   = _ * 2
}

class CurriedFunction2Test {
  @Test def tupled(): Unit = assertEquals(3, ((i: Int) ⇒ (j: Int) ⇒ i + j).tupled((1, 2)))
}

class PredicateTest {
  @Test def cond(): Unit = {
    assertEquals(List("even", "odd", "even", "even"), List(2, 3, 4, 6).map(isEven.cond("even", "odd")))
  }

  @Test def and(): Unit = {
    assertEquals(List(4, 6), List(2, 3, 4, 6).filter(isEven and (_ > 2)))
    assertEquals(List(4, 6), List(2, 3, 4, 6).filter(function.and(isEven, _ > 2)))
    assertTrue(function.and[Int]().apply(Random.nextInt()))
  }

  @Test def or(): Unit = {
    assertEquals(List(2, 4, 3), List(2, 1, 4, 3, 5).filter(isEven or (_ == 3)))
    assertEquals(List(2, 4, 3), List(2, 1, 4, 3, 5).filter(function.or(isEven, _ == 3)))
    assertFalse(function.or[Int]().apply(Random.nextInt()))
  }

  @Test def not(): Unit = assertEquals(List(1, 3, 5), List(2, 1, 4, 3, 5).filter(isEven.not))

  @Test def exists(): Unit = assertEquals(List(List(2), List(2, 4), List(2, 4, 3)),
    List(Nil, List(2), List(3), List(2, 4), List(2, 4, 3)).filter(isEven.exists))

  @Test def forall(): Unit = assertEquals(List(Nil, List(2), List(2, 4)),
    List(Nil, List(2), List(3), List(2, 4), List(2, 4, 3)).filter(isEven.forall))

  @Test def ifSome(): Unit = assertEquals(List(Some(4), Some(6)),
    List(None, Some(3), Some(4), None, Some(6)).filter(isEven.ifSome))

  @Test def guard(): Unit = on(1, 2, 3, 4).calling((isEven guard double).lift).produces(None, Some(4), None, Some(8))

  @Test def nand(): Unit = {
    assertEquals(List(2, 3), List(2, 3, 4, 6).filter(function.nand(isEven, _ > 2)))
    assertFalse(function.nand[Int]().apply(Random.nextInt()))
  }

  @Test def nor(): Unit = {
    assertEquals(List(1, 5), List(2, 1, 4, 3, 5).filter(function.nor(isEven, _ == 3)))
    assertTrue(function.nor[Int]().apply(Random.nextInt()))
  }

  private val isEven: Predicate[Int] = _ % 2 == 0
  private val double: (Int ⇒ Int)   = _ * 2
}

class PartialFunctionTest {
  @Test def either(): Unit  = on(1, 2).calling(util.partial(1 → "2").either).produces(Right("2"), Left(2))
  @Test def toRight(): Unit = on(1, 2).calling(util.partial(1 → "2").toRight).produces(Right("2"), Left(2))
  @Test def toLeft(): Unit  = on(1, 2).calling(util.partial(1 → "2").toLeft).produces(Left("2"), Right(2))
  @Test def unify(): Unit   = on(1, 2).calling(util.partial(2 → 4).unify).produces(1, 4)

  @Test def isUndefinedAt(): Unit =
    on("oof", "foo").calling(util.partial("foo" → "bar").isUndefinedAt).produces(true, false)

  @Test def first(): Unit =
    on(1 → "foo", 2 → "bar").calling(util.partial(1 → 2).first[String].lift).produces(Some(2 → "foo"), None)

  @Test def second(): Unit =
    on("foo" → 1, "bar" → 2).calling(util.partial(1 → 2).second[String].lift).produces(Some("foo" → 2), None)

  @Test def partition(): Unit =
    assertEquals((List(2, 4), List("one", "three")), util.partial(1 → "one", 3 → "three").partition(List(1, 2, 3, 4)))

  @Test def starStarStar(): Unit = {
    val composed = util.partial(1 → 2) *** util.partial(2 → 3)

    assertTrue(composed.isDefinedAt((1, 2)))
    assertFalse(composed.isDefinedAt((0, 2)))
    assertFalse(composed.isDefinedAt((1, 0)))
    assertEquals((2, 3), composed.apply((1, 2)))
  }
}
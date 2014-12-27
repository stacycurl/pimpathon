package pimpathon

import org.junit.Test
import scala.util.Random

import org.junit.Assert._
import pimpathon.function._


class FunctionTest {
  @Test def forall(): Unit = {
    assertEquals(List(Nil, List(2), List(2, 4)),
      List(Nil, List(2), List(3), List(2, 4), List(2, 4, 3)).filter(isEven.forall))
  }

  @Test def exists(): Unit = {
    assertEquals(List(List(2), List(2, 4), List(2, 4, 3)),
      List(Nil, List(2), List(3), List(2, 4), List(2, 4, 3)).filter(isEven.exists))
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

  @Test def nand(): Unit = {
    assertEquals(List(2, 3), List(2, 3, 4, 6).filter(function.nand(isEven, _ > 2)))
    assertFalse(function.nand[Int]().apply(Random.nextInt()))
  }

  @Test def not(): Unit = {
    assertEquals(List(1, 3, 5), List(2, 1, 4, 3, 5).filter(isEven.not))
  }

  @Test def ifSome(): Unit = {
    assertEquals(List(Some(4), Some(6)),
      List(None, Some(3), Some(4), None, Some(6)).filter(isEven.ifSome))
  }

  @Test def guard(): Unit = {
    assertEquals(List(None, Some(4), None, Some(8)), List(1, 2, 3, 4).map((isEven guard double).lift))
  }

  @Test def guardWith(): Unit = {
    assertEquals(List(None, Some(4), None, Some(8)), List(1, 2, 3, 4).map((double guardWith isEven).lift))
  }

  @Test def tupled(): Unit = {
    assertEquals(3, ((i: Int) => (j: Int) => i + j).tupled((1, 2)))
  }

  private val isEven: Predicate[Int] = _ % 2 == 0
  private val double: (Int => Int)   = _ * 2
}

class PartialFunctionTest {
  @Test def isUndefinedAt(): Unit = assertEquals(
    List(true, false), List("gibberish", "foo").map(util.partial("foo" -> "bar").isUndefinedAt)
  )

  @Test def first(): Unit = assertEquals(
    List(Some(2 -> "foo"), None), List(1 -> "foo", 2 -> "bar").map(util.partial(1 -> 2).first[String].lift)
  )

  @Test def second(): Unit = assertEquals(
    List(Some("foo" -> 2), None), List("foo" -> 1, "bar" -> 2).map(util.partial(1 -> 2).second[String].lift)
  )

  @Test def starStarStar(): Unit = {
    val composed = util.partial(1 -> 2) *** util.partial(2 -> 3)

    assertTrue(composed.isDefinedAt((1, 2)))
    assertFalse(composed.isDefinedAt((0, 2)))
    assertFalse(composed.isDefinedAt((1, 0)))
    assertEquals((2, 3), composed.apply((1, 2)))
  }

  @Test def either(): Unit = assertEquals(
    List(Right("2"), Left(5)),
    List(1, 5).map(util.partial(1 -> "2").either)
  )

  @Test def toRight(): Unit = assertEquals(
    List(Right("2"), Left(5)),
    List(1, 5).map(util.partial(1 -> "2").toRight)
  )

  @Test def toLeft(): Unit = assertEquals(
    List(Left("2"), Right(5)),
    List(1, 5).map(util.partial(1 -> "2").toLeft)
  )

  @Test def partition(): Unit = assertEquals(
    (List(2, 4), List("one", "three")),
    util.partial(1 -> "one", 3 -> "three").partition(List(1, 2, 3, 4))
  )

  @Test def unify(): Unit = assertEquals(
    List(1, 4), List(1, 2).map(util.partial(2 -> 4).unify)
  )
}

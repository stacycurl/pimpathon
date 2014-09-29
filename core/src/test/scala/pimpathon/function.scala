package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.function._


class FunctionTest {
  @Test def forall: Unit = {
    assertEquals(List(Nil, List(2), List(2, 4)),
      List(Nil, List(2), List(3), List(2, 4), List(2, 4, 3)).filter(isEven.forall))
  }

  @Test def exists: Unit = {
    assertEquals(List(List(2), List(2, 4), List(2, 4, 3)),
      List(Nil, List(2), List(3), List(2, 4), List(2, 4, 3)).filter(isEven.exists))
  }

  @Test def and: Unit = {
    assertEquals(List(4, 6), List(2, 3, 4, 6).filter(isEven and (_ > 2)))
  }

  @Test def or: Unit = {
    assertEquals(List(2, 4, 3), List(2, 1, 4, 3, 5).filter(isEven or (_ == 3)))
  }

  @Test def not: Unit = {
    assertEquals(List(1, 3, 5), List(2, 1, 4, 3, 5).filter(isEven.not))
  }

  @Test def ifSome: Unit = {
    assertEquals(List(Some(4), Some(6)),
      List(None, Some(3), Some(4), None, Some(6)).filter(isEven.ifSome))
  }

  private val isEven: (Int => Boolean) = (_ % 2 == 0)
}

class PartialFunctionTest {
  @Test def starStarStar: Unit = {
    val composed = util.partial(1 -> 2) *** util.partial(2 -> 3)

    assertTrue(composed.isDefinedAt((1, 2)))
    assertFalse(composed.isDefinedAt((0, 2)))
    assertFalse(composed.isDefinedAt((1, 0)))
    assertEquals((2, 3), composed.apply((1, 2)))
  }

  @Test def either: Unit = assertEquals(
    List(Right("2"), Left(5)),
    List(1, 5).map(util.partial(1 -> "2").either)
  )

  @Test def toRight: Unit = assertEquals(
    List(Right("2"), Left(5)),
    List(1, 5).map(util.partial(1 -> "2").toRight)
  )

}

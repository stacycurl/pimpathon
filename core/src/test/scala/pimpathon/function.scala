package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.function._


class FunctionTest {
  @Test def forall {
    assertEquals(List(Nil, List(2), List(2, 4)),
      List(Nil, List(2), List(3), List(2, 4), List(2, 4, 3)).filter(isEven.forall))
  }

  @Test def exists {
    assertEquals(List(List(2), List(2, 4), List(2, 4, 3)),
      List(Nil, List(2), List(3), List(2, 4), List(2, 4, 3)).filter(isEven.exists))
  }

  @Test def and {
    assertEquals(List(4, 6), List(2, 3, 4, 6).filter(isEven and (_ > 2)))
  }

  @Test def or {
    assertEquals(List(2, 4, 3), List(2, 1, 4, 3, 5).filter(isEven or (_ == 3)))
  }

  @Test def not {
    assertEquals(List(1, 3, 5), List(2, 1, 4, 3, 5).filter(isEven.not))
  }

  @Test def ifSome {
    assertEquals(List(Some(4), Some(6)),
      List(None, Some(3), Some(4), None, Some(6)).filter(isEven.ifSome))
  }

  private val isEven: (Int => Boolean) = (_ % 2 == 0)
}

package stacycurl.scala.pimpathon

import org.junit.Test

import org.junit.Assert._
import stacycurl.scala.pimpathon.function._


class FunctionTest {
  @Test def forall {
    assertEquals(List(Nil, List(2), List(2, 4)),
      List(Nil, List(2), List(3), List(2, 4), List(2, 4, 3)).filter(isEven.forall))
  }

  private val isEven: (Int => Boolean) = (_ % 2 == 0)
}

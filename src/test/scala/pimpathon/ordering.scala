package pimpathon

import org.junit.Test
import pimpathon.list.ListPimps
import pimpathon.ordering._
import pimpathon.util._


class OrderingTest {
  @Test def promote: Unit = on(List(1, 2), List(1, 9), List(9, 2), List(9, 10))
    .calling(_.sorted(Ordering.Int.promote(10, 9))).produces(List(1, 2), List(9, 1), List(9, 2), List(10, 9))

  @Test def demote: Unit = on(List(1, 2), List(1, 9), List(9, 2), List(9, 10))
    .calling(_.sorted(Ordering.Int.demote(2, 1))).produces(List(2, 1), List(9, 1), List(9, 2), List(9, 10))

  @Test def and: Unit =
    List((1, 2), (1, 3), (2, 3)).shuffle.sorted(Ordering.Int && Ordering.Int) === List((1, 2), (1, 3), (2, 3))

  @Test def or: Unit =
    List(4, 2, 1, 3).shuffle.sorted(bit(0) || bit(1)) === List(4, 2, 1, 3)
    
  @Test def sameAs: Unit = {
    List(1, 2, 3, 4, 5).sorted(Ordering.sameAs(4, 2, 1, 3)) === List(4, 2, 1, 3, 5)
  }

  private def bit(index: Int): Ordering[Int] = Ordering.Boolean.on[Int](i ⇒ (i & (1 << index)) != 0)
}
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
}
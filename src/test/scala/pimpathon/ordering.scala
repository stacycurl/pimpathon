package pimpathon

import org.junit.Test
import pimpathon.util._
import pimpathon.ordering._


class OrderingTest {
  @Test def promote: Unit = on(List(1, 2), List(1, 9), List(9, 2), List(9, 10))
    .calling(_.sorted(Ordering[Int].promote(10, 9))).produces(List(1, 2), List(9, 1), List(9, 2), List(10, 9))

  @Test def demote: Unit = on(List(1, 2), List(1, 9), List(9, 2), List(9, 10))
    .calling(_.sorted(Ordering[Int].demote(2, 1))).produces(List(2, 1), List(9, 1), List(9, 2), List(9, 10))
}
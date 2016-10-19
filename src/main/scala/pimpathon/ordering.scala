package pimpathon

import scala.math.Ordering._
import pimpathon.any._


object ordering {
  implicit class OrderingPimps[A](val o: Ordering[A]) extends AnyVal {
    def promote(as: A*): Ordering[A] = Tuple2[Option[Int], A](Option[Int](Int.reverse).reverse, o).on[A](index(as: _*))
    def demote(as: A*): Ordering[A]  = Tuple2[Option[Int], A](Option[Int](Int), o).on[A](index(as: _*))

    def &&[B](next: Ordering[B]): Ordering[(A, B)] = Ordering.Tuple2(o, next)

    private def index(as: A*)(a: A): (Option[Int], A) = (as.indexOf(a).filterSelf(_ >= 0), a)
  }
}
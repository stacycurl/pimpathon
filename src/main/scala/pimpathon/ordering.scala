package pimpathon

import scala.math.Ordering._
import pimpathon.any._


object ordering {
  implicit class OrderingPimps[A](val self: Ordering[A]) extends AnyVal {
    def promote(as: A*): Ordering[A] = Tuple2[Option[Int], A](Option[Int](Int.reverse).reverse, self).on[A](index(as: _*))
    def demote(as: A*): Ordering[A]  = Tuple2[Option[Int], A](Option[Int](Int), self).on[A](index(as: _*))

    def ||(next: Ordering[A]): Ordering[A] = &&(next).on[A](a ⇒ (a, a))
    def &&[B](next: Ordering[B]): Ordering[(A, B)] = Ordering.Tuple2(self, next)

    private def index(as: A*)(a: A): (Option[Int], A) = (as.indexOf(a).filterSelf(_ >= 0), a)
  }
}
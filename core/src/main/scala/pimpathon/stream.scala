package pimpathon

import pimpathon.function._


object stream {
  def cond[A](cond: Boolean, stream: ⇒ Stream[A]): Stream[A] = if (cond) stream else Stream.empty[A]
  def continuallyWhile[A](elem: ⇒ A)(p: Predicate[A]): Stream[A] = Stream.continually(elem).takeWhile(p)

  implicit class StreamPimps[A](val value: Stream[A]) extends AnyVal {
    def lazyScanLeft[B](z: B)(op: (B, A) ⇒ B): Stream[B] = {
      def loop(as: ⇒ Stream[A], acc: B): Stream[B] = acc #:: as.unconsC(Stream.empty[B], h ⇒ t ⇒ loop(t, op(acc, h)))

      loop(value, z)
    }

    def tailOption: Option[Stream[A]] = uncons(None, _ ⇒ Some(value.tail))
    def unconsC[B](empty: ⇒ B, nonEmpty: A ⇒ (⇒ Stream[A]) ⇒ B): B = uncons(empty, _ ⇒ nonEmpty(value.head)(value.tail))
    def uncons[B](empty: ⇒ B, nonEmpty: Stream[A] ⇒ B): B = if (value.isEmpty) empty else nonEmpty(value)
  }
}
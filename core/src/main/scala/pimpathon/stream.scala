package pimpathon

import pimpathon.function._


object stream {
  def cond[A](cond: Boolean, stream: ⇒ Stream[A]): Stream[A] = if (cond) stream else Stream.empty[A]
  def continuallyWhile[A](elem: ⇒ A)(p: Predicate[A]): Stream[A] = Stream.continually(elem).takeWhile(p)

  implicit def streamOps[A](stream: Stream[A]): StreamOps[A] = new StreamOps[A](stream)

  class StreamOps[A](val value: Stream[A]) {
    def uncons[B](empty: ⇒ B, nonEmpty: Stream[A] ⇒ B): B = if (value.isEmpty) empty else nonEmpty(value)

    def tailOption: Option[Stream[A]] = uncons(None, _ ⇒ Some(value.tail))
  }
}
package pimpathon


object stream {
  def cond[A](cond: Boolean, stream: => Stream[A]): Stream[A] = if (cond) stream else Stream.empty[A]
  def continuallyWhile[A](elem: => A)(p: A => Boolean): Stream[A] = Stream.continually(elem).takeWhile(p)
}


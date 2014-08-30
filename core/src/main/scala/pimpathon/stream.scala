package pimpathon


object stream {
  def continuallyWhile[A](elem: => A)(p: A => Boolean): Stream[A] =
    Stream.continually(elem).takeWhile(p)
}


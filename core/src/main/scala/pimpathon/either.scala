package pimpathon



object either {
  implicit class EitherOps[L, R](val either: Either[L, R]) extends AnyVal {
    def tap(l: L => Unit, r: R => Unit): Either[L, R] = { either.fold(l, r); either }
  }
}


package pimpathon



object either {
  implicit class EitherOps[L, R](val either: Either[L, R]) extends AnyVal {
    def map[LV, RV](lf: L => LV, rf: R => RV): Either[LV, RV] =
      either.fold(l => Left[LV, RV](lf(l)), r => Right[LV, RV](rf(r)))

    def tap(l: L => Unit, r: R => Unit): Either[L, R] = { either.fold(l, r); either }
  }
}


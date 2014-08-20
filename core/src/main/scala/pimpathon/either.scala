package pimpathon


object either {
  implicit def eitherOps[L, R](either: Either[L, R]): EitherOps[L, R] = new EitherOps[L, R](either)

  class EitherOps[L, R](either: Either[L, R]) {
    def map[LV, RV](lf: L => LV, rf: R => RV): Either[LV, RV] =
      either.fold(l => Left[LV, RV](lf(l)), r => Right[LV, RV](rf(r)))

    def leftOr(rl: R => L): L = either.fold(identity, rl)
    def rightOr(lr: L => R): R = either.fold(lr, identity)

    def tap(l: L => Unit, r: R => Unit): Either[L, R] = { either.fold(l, r); either }
  }
}


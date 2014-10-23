package pimpathon

import pimpathon.function._


object either {
  implicit def eitherOps[L, R](either: Either[L, R]): EitherOps[L, R] = new EitherOps[L, R](either)

  class EitherOps[L, R](either: Either[L, R]) {
    def bimap[LV, RV](lf: L => LV, rf: R => RV): Either[LV, RV] =
      either.fold(l => Left[LV, RV](lf(l)), r => Right[LV, RV](rf(r)))

    def leftMap[M](f: L => M): Either[M, R] = bimap[M, R](f, identity[R])
    def rightMap[S](f: R => S): Either[L, S] = bimap[L, S](identity[L], f)

    def leftFlatMap(f: L => Either[L, R]): Either[L, R] = either.fold(f, Right(_))
    def rightFlatMap(f: R => Either[L, R]): Either[L, R] = either.fold(Left(_), f)

    def leftOr(rl: R => L): L = either.fold(identity, rl)
    def rightOr(lr: L => R): R = either.fold(lr, identity)

    def valueOr(lr: L => R): R = rightOr(lr)
    def valueOr(pf: PartialFunction[L, R]): Either[L, R] = rescue(pf)

    def rescue(lr: L => R): R = rightOr(lr)
    def rescue(pf: PartialFunction[L, R]): Either[L, R] = leftFlatMap(pf.either)

    def tap(l: L => Unit, r: R => Unit): Either[L, R] = { either.fold(l, r); either }
  }

  implicit def eitherToRightProjection[L, R](either: Either[L, R]): Either.RightProjection[L, R] = either.right
  implicit def rightProjectionToEither[L, R](rp: Either.RightProjection[L, R]): Either[L, R] = rp.e
}

package pimpathon

import scala.util.{Failure, Success, Try}


object either {
  implicit class EitherOps[L, R](val either: Either[L, R]) extends AnyVal {
    def bimap[LV, RV](lf: L => LV, rf: R => RV): Either[LV, RV] =
      either.fold(l => Left[LV, RV](lf(l)), r => Right[LV, RV](rf(r)))

    def leftMap[M](f: L => M): Either[M, R] = bimap[M, R](f, identity[R])
    def rightMap[S](f: R => S): Either[L, S] = bimap[L, S](identity[L], f)

    def leftFlatMap(f: L => Either[L, R]): Either[L, R] = either.fold(f, Right(_))
    def rightFlatMap(f: R => Either[L, R]): Either[L, R] = either.fold(Left(_), f)

    def leftOr(rl: R => L): L = either.fold(identity, rl)
    def rightOr(lr: L => R): R = either.fold(lr, identity)

    def tap(l: L => Unit, r: R => Unit): Either[L, R] = { either.fold(l, r); either }

    def toTry(implicit ev: L <:< Throwable): Try[R] = either.fold(Failure(_), Success(_))
  }

  implicit def eitherToRightProjection[L, R](either: Either[L, R]): Either.RightProjection[L, R] = either.right
  implicit def rightProjectionToEither[L, R](rp: Either.RightProjection[L, R]): Either[L, R] = rp.e
}

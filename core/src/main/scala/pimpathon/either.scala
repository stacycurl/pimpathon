package pimpathon

import scala.util.{Failure, Success, Try}


object either {
  implicit class EitherOps[L, R](val either: Either[L, R]) extends AnyVal {
    def map[LV, RV](lf: L => LV, rf: R => RV): Either[LV, RV] =
      either.fold(l => Left[LV, RV](lf(l)), r => Right[LV, RV](rf(r)))

    def leftMap[M](f: L => M): Either[M, R] = map[M, R](f, identity[R])
    def rightMap[S](f: R => S): Either[L, S] = map[L, S](identity[L], f)

    def leftOr(rl: R => L): L = either.fold(identity, rl)
    def rightOr(lr: L => R): R = either.fold(lr, identity)

    def tap(l: L => Unit, r: R => Unit): Either[L, R] = { either.fold(l, r); either }

    def toTry(implicit ev: L <:< Throwable): Try[R] = either.fold(Failure(_), Success(_))
  }
}


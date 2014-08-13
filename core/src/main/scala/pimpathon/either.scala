package pimpathon

import scala.util.{Failure, Success, Try}


object either {
  implicit class EitherOps[L, R](val either: Either[L, R]) extends AnyVal {
    def map[LV, RV](lf: L => LV, rf: R => RV): Either[LV, RV] =
      either.fold(l => Left[LV, RV](lf(l)), r => Right[LV, RV](rf(r)))

    def tap(l: L => Unit, r: R => Unit): Either[L, R] = { either.fold(l, r); either }

    def toTry(implicit ev: L <:< Throwable): Try[R] = either.fold(Failure(_), Success(_))
  }
}


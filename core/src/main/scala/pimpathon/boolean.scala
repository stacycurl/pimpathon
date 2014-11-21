package pimpathon


object boolean {
  implicit def booleanOps(value: Boolean): BooleanOps = new BooleanOps(value)

  class BooleanOps(val value: Boolean) {
    def either[R](right: R): EitherCapturer[R] = new EitherCapturer[R](value, right)
  }

  class EitherCapturer[R](value: Boolean, right: R) {
    def or[L](left: => L): Either[L, R] = if (value) Right(right) else Left(left)
  }
}

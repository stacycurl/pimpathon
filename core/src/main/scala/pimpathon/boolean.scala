package pimpathon


object boolean {
  implicit class BooleanPimps(val value: Boolean) extends AnyVal {
    def asInt: Int = if (value) 1 else 0
    def either[R](right: R): EitherCapturer[R] = new EitherCapturer[R](value, right)
    def option[A](a: ⇒ A): Option[A] = if (value) Some(a) else None
    def implies(rhs: Boolean): Boolean = !value || rhs
  }

  class EitherCapturer[R](value: Boolean, right: R) {
    def or[L](left: ⇒ L): Either[L, R] = if (value) Right(right) else Left(left)
  }
}
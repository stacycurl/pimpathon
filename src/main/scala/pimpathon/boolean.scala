package pimpathon


object boolean {
  implicit class BooleanPimps(val self: Boolean) extends AnyVal {
    def asInt: Int = if (self) 1 else 0
    def either[R](right: R): EitherCapturer[R] = new EitherCapturer[R](self, right)
    def option[A](a: ⇒ A): Option[A] = if (self) Some(a) else None
    def implies(rhs: Boolean): Boolean = !self || rhs
    def nor(rhs: Boolean): Boolean = !(self || rhs)
    def nand(rhs: Boolean): Boolean = !(self && rhs)
    def cond[A](ifTrue: ⇒ A, ifFalse: ⇒ A): A = if (self) ifTrue else ifFalse

    def tapFalse[Discarded](ifFalse: ⇒ Discarded): Boolean = {
      if (!self) ifFalse
      self
    }

    def tapTrue[Discarded](ifTrue: ⇒ Discarded): Boolean = {
      if (self) ifTrue
      self
    }
  }

  class EitherCapturer[R](value: Boolean, right: R) {
    def or[L](left: ⇒ L): Either[L, R] = if (value) Right(right) else Left(left)
  }
}
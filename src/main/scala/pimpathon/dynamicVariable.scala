package pimpathon

import scala.util.DynamicVariable

object dynamicVariable {
  implicit class DynamicVariablePimps[A](val self: DynamicVariable[A]) extends AnyVal {
    def modify(f: A => A): DynamicVariable[A] = {
      self.value = f(self.value)
      self
    }

    def withModification[B](f: A => A)(thunk: => B): B = {
      self.withValue(f(self.value))(thunk)
    }
  }
}

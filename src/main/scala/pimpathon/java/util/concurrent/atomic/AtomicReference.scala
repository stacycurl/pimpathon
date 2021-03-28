package pimpathon.java.util.concurrent.atomic

import java.util.concurrent.atomic.AtomicReference
import pimpathon.either.EitherPimps


object atomicReference {
  implicit class AtomicReferencePimps[A](private val self: AtomicReference[A]) extends AnyVal {
    def update(f: A ⇒ A): A = self.updateAndGet((a: A) ⇒ f(a))
    def updateEither[L](f: A ⇒ Either[L, A]): Either[L, A] = f(self.get()).tapRight(self.set)
  }  
}

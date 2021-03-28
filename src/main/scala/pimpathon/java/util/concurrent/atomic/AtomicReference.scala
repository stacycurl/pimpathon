package pimpathon.java.util.concurrent.atomic

import java.util.concurrent.atomic.AtomicReference


object atomicReference {
  implicit class AtomicReferencePimps[A](private val self: AtomicReference[A]) extends AnyVal {
    def update(f: A ⇒ A): A = self.updateAndGet((a: A) ⇒ f(a))
  }  
}

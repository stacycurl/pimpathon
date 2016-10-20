package pimpathon

import scala.collection.generic.CanBuildFrom
import scala.collection.{mutable ⇒ M}

import pimpathon.any._


object builder {
  implicit class CanBuildFromPimps[Elem, To](cbf: CanBuildFrom[Nothing, Elem, To]) {
    def mapResult[X](f: To ⇒ X): CanBuildFrom[Nothing, Elem, X] = new CanBuildFrom[Nothing, Elem, X] {
      def apply(from: Nothing): M.Builder[Elem, X] = cbf.apply(from).mapResult(f)
      def apply(): M.Builder[Elem, X] = cbf.apply().mapResult(f)
    }

    def on[C](f: C ⇒ Elem): CanBuildFrom[Nothing, C, To] = new CanBuildFrom[Nothing, C, To] {
      def apply(from: Nothing): M.Builder[C, To] = cbf.apply(from).on(f)
      def apply(): M.Builder[C, To] = cbf.apply().on(f)
    }
  }

  implicit class BuilderPimps[A, B](val builder: M.Builder[A, B]) extends AnyVal {
    def +++=(xss: TraversableOnce[TraversableOnce[A]]): M.Builder[A, B] = builder.tap(b ⇒ xss.foreach(b ++= _))
    def on[C](f: C ⇒ A): M.Builder[C, B] = new ContramappedBuilder(builder, f)
    def run[Discarded](actions: (M.Builder[A, B] ⇒ Discarded)*): B = builder.tap(actions: _*).reset()
    def reset(): B = builder.result().tap(_ ⇒ builder.clear())
  }

  private class ContramappedBuilder[A, B, C](builder: M.Builder[A, B], f: C ⇒ A) extends M.Builder[C, B] {
    def +=(elem: C): this.type = { builder += f(elem); this }
    def result(): B = builder.result()
    def clear(): Unit = builder.clear()
  }
}
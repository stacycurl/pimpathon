package pimpathon

import scala.collection.{mutable ⇒ M}

import pimpathon.any._


object builder {
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
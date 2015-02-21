package pimpathon

import scala.collection.generic.{Growable, Shrinkable}


object tuple {
  implicit def tuple2Ops[A, B](t: (A, B)): Tuple2Ops[A, B] = new Tuple2Ops[A, B](t)

  class Tuple2Ops[A, B](val t: (A, B)) {
    def addTo(as: Growable[A], bs: Growable[B]): (A, B) = tap(a ⇒ b ⇒ {as += a; bs += b })
    def removeFrom(as: Shrinkable[A], bs: Shrinkable[B]): (A, B) = tap(a ⇒ b ⇒ {as -= a; bs -= b })
    def tap[Discarded](actions: (A ⇒ B ⇒ Discarded)*): (A, B) = { actions.foreach(a ⇒ a(t._1)(t._2)); t }
    def calc[C](f: (A, B) ⇒ C): C = f(t._1, t._2)
    def calcC[C](f: A ⇒ B ⇒ C): C = f(t._1)(t._2)
    def to[C](implicit ac: A ⇒ C, bc: B ⇒ C): (C, C) = (ac(t._1), bc(t._2))
    def tmap[C, D](f: A ⇒ C, g: B ⇒ D): (C, D) = (f(t._1), g(t._2))
  }
}
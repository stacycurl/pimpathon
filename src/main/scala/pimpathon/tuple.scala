package pimpathon

import scala.collection.generic.{Growable, Shrinkable}


object tuple {
  implicit class Tuple2Pimps[A, B](val self: (A, B)) extends AnyVal {
    def addTo(as: Growable[A], bs: Growable[B]): (A, B) = tap(a ⇒ b ⇒ {as += a; bs += b })
    def removeFrom(as: Shrinkable[A], bs: Shrinkable[B]): (A, B) = tap(a ⇒ b ⇒ {as -= a; bs -= b })
    def tap[Discarded](actions: (A ⇒ B ⇒ Discarded)*): (A, B) = { actions.foreach(a ⇒ a(self._1)(self._2)); self }
    def calc[C](f: (A, B) ⇒ C): C = f(self._1, self._2)
    def calcC[C](f: A ⇒ B ⇒ C): C = f(self._1)(self._2)
    def to[C](implicit ac: A ⇒ C, bc: B ⇒ C): (C, C) = (ac(self._1), bc(self._2))
    def tmap[C, D](f: A ⇒ C, g: B ⇒ D): (C, D) = (f(self._1), g(self._2))
    def map1[C](f: A ⇒ C): (C, B) = calcC(a ⇒ b ⇒ (f(a), b))
    def map2[C](f: B ⇒ C): (A, C) = calcC(a ⇒ b ⇒ (a, f(b)))
  }
}
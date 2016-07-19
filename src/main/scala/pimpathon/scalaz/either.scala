package pimpathon.scalaz

import scala.collection.generic.{Shrinkable, Growable}
import scalaz.{-\/, \/-, \/}


object either {
  implicit class DisjunctionFrills[L, R](val disjunction: L \/ R) extends AnyVal {
    def addTo(ls: Growable[L], rs: Growable[R]): L \/ R = tap(ls += _, rs += _)
    def removeFrom(ls: Shrinkable[L], rs: Shrinkable[R]): L \/ R = tap(ls -= _, rs -= _)

    def tapLeft[Discarded](l: L ⇒ Discarded): L \/ R = tap(l, _ ⇒ {})
    def tapRight[Discarded](r: R ⇒ Discarded): L \/ R = tap(_ ⇒ {}, r)
    def tap[Discarded](l: L ⇒ Discarded, r: R ⇒ Discarded): L \/ R = { disjunction.fold(l, r); disjunction }

    def leftFlatMap(f: L ⇒ L \/ R): L \/ R  = disjunction.fold(f, \/-(_))
  }

  implicit class DisjunctionFrillsNestedL[L, R](val disjunction: (L \/ R) \/ R) {
    def flatten: L \/ R = disjunction.fold(identity, \/-(_))
  }

  implicit class DisjunctionFrillsNestedR[L, R](val disjunction: L \/ (L \/ R)) {
    def flatten: L \/ R = disjunction.fold(-\/(_), identity)
  }
}
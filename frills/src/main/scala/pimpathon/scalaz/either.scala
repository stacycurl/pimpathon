package pimpathon.scalaz

import scalaz.\/


object either {
  implicit class DisjunctionFrills[L, R](val disjunction: L \/ R) extends AnyVal {
    def tap[Discarded](l: L ⇒ Discarded, r: R ⇒ Discarded): L \/ R = { disjunction.fold(l, r); disjunction }
  }
}
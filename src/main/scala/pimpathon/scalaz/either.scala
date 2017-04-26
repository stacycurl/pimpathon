package pimpathon.scalaz

import scala.collection.generic.{Growable, Shrinkable}
import scala.util.{Failure, Success, Try}
import scalaz.{-\/, \/, \/-}


object either {
  implicit class DisjunctionFrills[L, R](val self: L \/ R) extends AnyVal {
    def addTo(ls: Growable[L], rs: Growable[R]): L \/ R = tap(ls += _, rs += _)
    def removeFrom(ls: Shrinkable[L], rs: Shrinkable[R]): L \/ R = tap(ls -= _, rs -= _)

    def tapLeft[Discarded](l: L ⇒ Discarded): L \/ R = tap(l, _ ⇒ {})
    def tapRight[Discarded](r: R ⇒ Discarded): L \/ R = tap(_ ⇒ {}, r)
    def tap[Discarded](l: L ⇒ Discarded, r: R ⇒ Discarded): L \/ R = { self.fold(l, r); self }

    def leftFlatMap(f: L ⇒ L \/ R): L \/ R  = self.fold(f, \/-(_))

    def getMessage(implicit ev: L <:< Throwable): Option[String] = self.fold(t ⇒ Some(t.getMessage), _ ⇒ None)
    def toTry(implicit ev: L <:< Throwable): Try[R] = self.fold(Failure(_), Success(_))
  }

  implicit class DisjunctionFrillsNestedL[L, R](val self: (L \/ R) \/ R) {
    def flatten: L \/ R = self.fold(identity, \/-(_))
  }

  implicit class DisjunctionFrillsNestedR[L, R](val self: L \/ (L \/ R)) {
    def flatten: L \/ R = self.fold(-\/(_), identity)
  }
}
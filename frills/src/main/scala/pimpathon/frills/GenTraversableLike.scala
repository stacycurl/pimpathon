package pimpathon.frills

import pimpathon.CCBF

import scala.language.{higherKinds, implicitConversions, reflectiveCalls}
import scala.collection.GenTraversableLike
import scalaz.\/

import pimpathon.scalaz.either._
import pimpathon.tuple._


trait genTraversableLike[CC[A]] {
  implicit def genTraversableLikeOfDisjunctionFrills[L, R, Repr](gtl: GenTraversableLike[L \/ R, Repr])
    : GenTraversableLikeOfDisjunctionFrills[L, R, Repr] = new GenTraversableLikeOfDisjunctionFrills[L, R, Repr](gtl)

  class GenTraversableLikeOfDisjunctionFrills[L, R, Repr](gtl: GenTraversableLike[L \/ R, Repr]) {
    def partitionDisjunctions[That[_]](implicit lcbf: CCBF[L, That], rcbf: CCBF[R, That]): (That[L], That[R]) =
      (lcbf.apply(), rcbf.apply()).tap(l ⇒ r ⇒ gtl.foreach(_.addTo(l, r))).tmap(_.result(), _.result())
  }
}
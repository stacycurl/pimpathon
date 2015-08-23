package pimpathon.frills

import pimpathon.CCBF

import scala.language.{higherKinds, implicitConversions, reflectiveCalls}
import scala.collection.{GenTraversable, GenTraversableLike}
import scalaz.\/

import pimpathon.scalaz.either._
import pimpathon.tuple._


object genTraversableLike {
  trait GenTraversableLikeOfDisjunctionFrillsMixin[L, R] {
    def partitionDisjunctions[That[_]](implicit lcbf: CCBF[L, That], rcbf: CCBF[R, That]): (That[L], That[R]) =
      (lcbf.apply(), rcbf.apply()).tap(l ⇒ r ⇒ gtl.foreach(_.addTo(l, r))).tmap(_.result(), _.result())

    protected def gtl: GenTraversableLike[L \/ R, GenTraversable[L \/ R]]
  }

  implicit class GenTraversableLikeOfDisjunctionFrills[L, R](
    protected val gtl: GenTraversableLike[L \/ R, GenTraversable[L \/ R]]
  ) extends GenTraversableLikeOfDisjunctionFrillsMixin[L, R]
}
package pimpathon.frills

import pimpathon.CCBF
import pimpathon.genTraversableLike.GenTraversableLikePimpsMixin

import scala.language.{higherKinds, implicitConversions, reflectiveCalls}
import scala.collection.{GenTraversable, GenTraversableLike}
import scalaz.\/

import pimpathon.scalaz.either._
import pimpathon.tuple._
import scalaz.syntax.std.either._
import scalaz.syntax.std.option._


object genTraversableLike {
  trait GenTraversableLikeFrillsMixin[A, CC[_]] extends GenTraversableLikePimpsMixin[A, CC] {
    def onlyOrDisjunction[B](f: CC[A] ⇒ B): B \/ A = onlyOption.toRightDisjunction(f(cc))
    def onlyDisjunction: CC[A] \/ A  = onlyEither.disjunction
  }

  trait GenTraversableLikeOfDisjunctionFrillsMixin[L, R] {
    def partitionDisjunctions[That[_]](implicit lcbf: CCBF[L, That], rcbf: CCBF[R, That]): (That[L], That[R]) =
      (lcbf.apply(), rcbf.apply()).tap(l ⇒ r ⇒ gtl.foreach(_.addTo(l, r))).tmap(_.result(), _.result())

    protected def gtl: GenTraversableLike[L \/ R, GenTraversable[L \/ R]]
  }

  implicit class GenTraversableLikeOfDisjunctionFrills[L, R](
    self: GenTraversableLike[L \/ R, GenTraversable[L \/ R]]
  ) extends GenTraversableLikeOfDisjunctionFrillsMixin[L, R] {

    protected def gtl: GenTraversableLike[L \/ R, GenTraversable[L \/ R]] = self
  }
}
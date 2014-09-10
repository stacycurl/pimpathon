package pimpathon

import scala.annotation.tailrec
import scala.collection.{breakOut, GenTraversableLike}
import scala.collection.breakOut
import scala.collection.generic.{CanBuildFrom, FilterMonadic}
import scala.collection.immutable._

import pimpathon.any._
import pimpathon.function._
import pimpathon.multiMap._
import pimpathon.option._
import pimpathon.tuple._


object genTraversableLike extends genTraversableLike

trait genTraversableLike {
  implicit def genTraversableLikeOps[A, Repr](gtl: GenTraversableLike[A, Repr])
    : GenTraversableLikeOps[A, Repr] = new GenTraversableLikeOps[A, Repr](gtl)

  class GenTraversableLikeOps[A, Repr](val list: GenTraversableLike[A, Repr]) {
    def asMap: GenTraversableLikeCapturer[A, Map, Repr] = as[Map]

    def asMultiMap[F[_]]: GenTraversableLikeCapturer[A, ({ type MM[K, V] = MultiMap[F, K, V] })#MM, Repr] =
      as[({ type MM[K, V] = MultiMap[F, K, V] })#MM]

    def as[F[_, _]]: GenTraversableLikeCapturer[A, F, Repr] = new GenTraversableLikeCapturer[A, F, Repr](list)

    def attributeCounts[B](f: A => B): Map[B, Int] =
      asMultiMap.withKeys(f).mapValues(_.size)

    def collectAttributeCounts[B](pf: PartialFunction[A, B]): Map[B, Int] =
      optAttributeCounts(pf.lift)

    def optAttributeCounts[B](f: A => Option[B]): Map[B, Int] =
      asMultiMap.withSomeKeys(f).mapValues(_.size)
  }
}

class GenTraversableLikeCapturer[A, F[_, _], Repr](list: GenTraversableLike[A, Repr]) {
  type CBF[K, V] = CanBuildFrom[Nothing, (K, V), F[K, V]]

  def withKeys[K](f: A => K)(implicit cbf: CBF[K, A]): F[K, A] = list.map(a => (f(a), a))(breakOut)
  def withValues[V](f: A => V)(implicit cbf: CBF[A, V]): F[A, V] = list.map(a => (a, f(a)))(breakOut)

  def withSomeKeys[K](f: A => Option[K])(implicit cbf: CBF[K, A]): F[K, A] =
    list.flatMap(a => f(a).map(_ -> a))(breakOut)

  def withSomeValues[V](f: A => Option[V])(implicit cbf: CBF[A, V]): F[A, V] =
    list.flatMap(a => f(a).map(a -> _))(breakOut)

  def withPFKeys[K](pf: PartialFunction[A, K])(implicit cbf: CBF[K, A]): F[K, A] =
    list.collect { case a if pf.isDefinedAt(a) => (pf(a), a) }(breakOut)

  def withPFValues[V](pf: PartialFunction[A, V])(implicit cbf: CBF[A, V]): F[A, V] =
    list.collect { case a if pf.isDefinedAt(a) => (a, pf(a)) }(breakOut)

  def withManyKeys[K](f: A => List[K])(implicit cbf: CBF[K, A]): F[K, A] =
    list.flatMap(a => f(a).map(_ -> a))(breakOut)
}

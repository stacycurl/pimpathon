package pimpathon

import scala.language.{higherKinds, implicitConversions}

import scala.collection.{breakOut, mutable => M, GenTraversable, GenTraversableLike}
import scala.collection.generic.CanBuildFrom

import pimpathon.any._
import pimpathon.function._
import pimpathon.map._
import pimpathon.multiMap._
import pimpathon.tuple._


object genTraversableLike extends genTraversableLike[
  ({ type CC[A] = GenTraversableLike[A, GenTraversable[A]] })#CC
]

trait genTraversableLike[CC[A] <: GenTraversableLike[A, GenTraversable[A]]]  {
  implicit def genTraversableLikeOps[A](gtl: CC[A])
    : GenTraversableLikeOps[A] = new GenTraversableLikeOps[A](gtl)

  implicit def genTraversableLikeOfEitherOps[L, R, Repr](gtl: GenTraversableLike[Either[L, R], Repr])
    : GenTraversableLikeOfEitherOps[L, R, Repr] = new GenTraversableLikeOfEitherOps[L, R, Repr](gtl)

  implicit def genTraversableLikeOfTuple2[K, V, Repr](gtl: GenTraversableLike[(K, V), Repr])
    : GenTraversableLikeOfTuple2[K, V, Repr] = new GenTraversableLikeOfTuple2[K, V, Repr](gtl)

  class GenTraversableLikeOps[A](val gtl: CC[A]) {
    def asMap: GenTraversableLikeCapturer[A, Map] = as[Map]

    def attributeCounts[B](f: A => B): Map[B, Int] =
      asMultiMap.withKeys(f).mapValues(_.size)

    def collectAttributeCounts[B](pf: PartialFunction[A, B]): Map[B, Int] =
      optAttributeCounts(pf.lift)

    def optAttributeCounts[B](f: A => Option[B]): Map[B, Int] =
      asMultiMap.withSomeKeys(f).mapValues(_.size)

    def asMultiMap[F[_]]: GenTraversableLikeCapturer[A, ({ type MM[K, V] = MultiMap[F, K, V] })#MM] =
      as[({ type MM[K, V] = MultiMap[F, K, V] })#MM]

    def as[F[_, _]]: GenTraversableLikeCapturer[A, F] = new GenTraversableLikeCapturer[A, F](gtl)

    def ungroupBy[B](f: A => B)(implicit inner: CCBF[A, CC], outer: CCBF[CC[A], CC]): CC[CC[A]] =
      gtl.foldLeft(UngroupBy[A, B, CC](Map(), Map())) { case (ungroupBy, item) => ungroupBy.add(item, f(item)) }.values

    def partitionByPF[B](pf: PartialFunction[A, B])
      (implicit eab: CCBF[Either[A, B], CC], a: CCBF[A, CC], b: CCBF[B, CC]): (CC[A], CC[B]) = pf.partition[CC](gtl)
  }

  class GenTraversableLikeOfEitherOps[L, R, Repr](gtl: GenTraversableLike[Either[L, R], Repr]) {
    def partitionEithers[That[T]](implicit lcbf: CCBF[L, That], rcbf: CCBF[R, That]): (That[L], That[R]) =
      (lcbf.apply(), rcbf.apply()).tap(lr => gtl.foreach(_.fold(lr._1 += _, lr._2 += _))).tmap(_.result(), _.result())
  }

  class GenTraversableLikeOfTuple2[K, V, Repr](gtl: GenTraversableLike[(K, V), Repr]) {
    def toMultiMap[F[_]](implicit fcbf: CCBF[V, F]): MultiMap[F, K, V] = gtl.map(kv => kv)(breakOut)
  }
}

class GenTraversableLikeCapturer[A, F[_, _]](gtl: GenTraversableLike[A, GenTraversable[A]]) {
  type CBF[K, V] = CanBuildFrom[Nothing, (K, V), F[K, V]]

  def withKeys[K](f: A => K)(implicit cbf: CBF[K, A]): F[K, A]   = withEntries(a => (f(a), a))
  def withValues[V](f: A => V)(implicit cbf: CBF[A, V]): F[A, V] = withEntries(a => (a, f(a)))
  def withEntries[K, V](f: A => ((K, V)))(implicit cbf: CBF[K, V]): F[K, V] = gtl.map(f)(breakOut)

  def withSomeKeys[K](f: A => Option[K])(implicit cbf: CBF[K, A]): F[K, A] =
    gtl.flatMap(a => f(a).map(_ -> a))(breakOut)

  def withSomeValues[V](f: A => Option[V])(implicit cbf: CBF[A, V]): F[A, V] =
    gtl.flatMap(a => f(a).map(a -> _))(breakOut)

  def withPFKeys[K](pf: PartialFunction[A, K])(implicit cbf: CBF[K, A]): F[K, A] =
    gtl.collect { case a if pf.isDefinedAt(a) => (pf(a), a) }(breakOut)

  def withPFValues[V](pf: PartialFunction[A, V])(implicit cbf: CBF[A, V]): F[A, V] =
    gtl.collect { case a if pf.isDefinedAt(a) => (a, pf(a)) }(breakOut)

  def withManyKeys[K](f: A => List[K])(implicit cbf: CBF[K, A]): F[K, A] =
    gtl.flatMap(a => f(a).map(_ -> a))(breakOut)
}

case class UngroupBy[A, B, CC[_]](ungrouped: Map[Int, M.Builder[A, CC[A]]], counts: Map[B, Int])(
  implicit inner: CCBF[A, CC], outer: CCBF[CC[A], CC]) {

  def add(a: A, b: B): UngroupBy[A, B, CC] = copy(ungrouped + entry(count(b), a), counts + ((b, count(b))))
  def values: CC[CC[A]] = ungrouped.sorted.values.map(_.result())(breakOut(outer))

  private def entry(count: Int, a: A) = (count, ungrouped.getOrElse(count, inner.apply()) += a)
  private def count(b: B) = counts.getOrElse(b, 0) + 1
}

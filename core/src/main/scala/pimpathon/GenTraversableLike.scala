package pimpathon

import scala.language.{higherKinds, implicitConversions, reflectiveCalls}

import scala.annotation.tailrec
import scala.collection.{breakOut, mutable ⇒ M, GenTraversable, GenTraversableLike}
import scala.collection.generic.CanBuildFrom

import pimpathon.any._
import pimpathon.boolean._
import pimpathon.either._
import pimpathon.function._
import pimpathon.map._
import pimpathon.multiMap._
import pimpathon.tuple._


object genTraversableLike extends genTraversableLike[({ type CC[A] = GenTraversableLike[A, GenTraversable[A]] })#CC] {
  protected def toGTL[A](gtl: GenTraversableLike[A, GenTraversable[A]]): GenTraversableLike[A, GenTraversable[A]] = gtl
}

abstract class genTraversableLike[CC[A]] {
  implicit def genTraversableLikeOps[A](cc: CC[A]): GenTraversableLikeOps[A] = new GenTraversableLikeOps[A](toGTL(cc))

  implicit def genTraversableLikeOfEitherOps[L, R, Repr](gtl: GenTraversableLike[Either[L, R], Repr])
    : GenTraversableLikeOfEitherOps[L, R, Repr] = new GenTraversableLikeOfEitherOps[L, R, Repr](gtl)

  implicit def genTraversableLikeOfTuple2[K, V, Repr](gtl: GenTraversableLike[(K, V), Repr])
    : GenTraversableLikeOfTuple2[K, V, Repr] = new GenTraversableLikeOfTuple2[K, V, Repr](gtl)

  class GenTraversableLikeOps[A](gtl: GenTraversableLike[A, GenTraversable[A]]) {
    def asMap: GenTraversableLikeCapturer[A, Map] = as[Map]

    def attributeCounts[B](f: A ⇒ B): Map[B, Int] =
      asMultiMap.withKeys(f).mapValues(_.size)

    def collectAttributeCounts[B](pf: PartialFunction[A, B]): Map[B, Int] =
      optAttributeCounts(pf.lift)

    def optAttributeCounts[B](f: A ⇒ Option[B]): Map[B, Int] =
      asMultiMap.withSomeKeys(f).mapValues(_.size)

    def asMultiMap[F[_]]: GenTraversableLikeCapturer[A, ({ type MM[K, V] = MultiMap[F, K, V] })#MM] =
      as[({ type MM[K, V] = MultiMap[F, K, V] })#MM]

    def as[F[_, _]]: GenTraversableLikeCapturer[A, F] = new GenTraversableLikeCapturer[A, F](gtl)

    def ungroupBy[B](f: A ⇒ B)(implicit inner: CCBF[A, CC], outer: CCBF[CC[A], CC]): CC[CC[A]] =
      gtl.foldLeft(UngroupBy[A, B, CC](Map(), Map())) { case (ungroupBy, item) ⇒ ungroupBy.add(item, f(item)) }.values

    def partitionByPF[B](pf: PartialFunction[A, B])
      (implicit eab: CCBF[Either[A, B], CC], a: CCBF[A, CC], b: CCBF[B, CC]): (CC[A], CC[B]) = pf.partition[CC](gtl)

    def seqMap[B, To](f: A ⇒ Option[B])(implicit cbf: CanBuildFrom[Nothing, B, To]): Option[To] =
      seqFold[M.Builder[B, To]](cbf())((builder, a) ⇒ f(a).map(builder += _)).map(_.result())

    def seqFold[B](z: B)(op: (B, A) ⇒ Option[B]): Option[B] = // similar to scalaz' GTL.foldLeftM[Option, B, A]
      apoFold[B, B](z)((b, a) ⇒ op(b, a).toRight(b)).right.toOption

    def apoFold[B, C](z: B)(op: (B, A) ⇒ Either[C, B]): Either[C, B] = {
      @tailrec def recurse(cur: GenTraversableLike[A, GenTraversable[A]], acc: B): Either[C, B] = cur.headOption match {
        case None ⇒ Right(acc)
        case Some(a) ⇒ op(acc, a) match {
          case Right(b) ⇒ recurse(cur.tail, b)
          case done     ⇒ done
        }
      }

      recurse(gtl, z)
    }
  }

  class GenTraversableLikeOfEitherOps[L, R, Repr](gtl: GenTraversableLike[Either[L, R], Repr]) {
    def partitionEithers[That[_]](implicit lcbf: CCBF[L, That], rcbf: CCBF[R, That]): (That[L], That[R]) =
      (lcbf.apply(), rcbf.apply()).tap(l ⇒ r ⇒ gtl.foreach(_.addTo(l, r))).tmap(_.result(), _.result())
  }

  class GenTraversableLikeOfTuple2[K, V, Repr](gtl: GenTraversableLike[(K, V), Repr]) {
    def toMultiMap[F[_]](implicit fcbf: CCBF[V, F]): MultiMap[F, K, V] = gtl.map(kv ⇒ kv)(breakOut)
  }

  protected def toGTL[A](cc: CC[A]): GenTraversableLike[A, GenTraversable[A]]
}

class GenTraversableLikeCapturer[A, F[_, _]](gtl: GenTraversableLike[A, GenTraversable[A]]) {
  import pimpathon.genTraversableLike._
  type CBF[K, V] = CanBuildFrom[Nothing, (K, V), F[K, V]]

  def withKeys[K](f: A ⇒ K)(implicit cbf: CBF[K, A]): F[K, A]    = withEntries(a ⇒ (f(a), a))
  def withValues[V](f: A ⇒ V)(implicit cbf: CBF[A, V]): F[A, V]  = withEntries(a ⇒ (a, f(a)))
  def withConstValue[V](v: V)(implicit  cbf: CBF[A, V]): F[A, V] = withEntries(a ⇒ (a, v))
  def withEntries[K, V](f: A ⇒ ((K, V)))(implicit cbf: CBF[K, V]): F[K, V] = gtl.map(f)(breakOut)

  def withSomeKeys[K](f: A ⇒ Option[K])(implicit cbf: CBF[K, A]): F[K, A] =
    gtl.flatMap(a ⇒ f(a).map(_ → a))(breakOut)

  def withSomeValues[V](f: A ⇒ Option[V])(implicit cbf: CBF[A, V]): F[A, V] =
    gtl.flatMap(a ⇒ f(a).map(a → _))(breakOut)

  def withPFKeys[K](pf: PartialFunction[A, K])(implicit cbf: CBF[K, A]): F[K, A] =
    gtl.collect { case a if pf.isDefinedAt(a) ⇒ (pf(a), a) }(breakOut)

  def withPFValues[V](pf: PartialFunction[A, V])(implicit cbf: CBF[A, V]): F[A, V] =
    gtl.collect { case a if pf.isDefinedAt(a) ⇒ (a, pf(a)) }(breakOut)

  def withManyKeys[K](f: A ⇒ List[K])(implicit cbf: CBF[K, A]): F[K, A] =
    gtl.flatMap(a ⇒ f(a).map(_ → a))(breakOut)

  def withUniqueKeys[K](f: A ⇒ K)(implicit cbf: CBF[K, A]): Option[F[K, A]] = {
    gtl.seqFold[(Set[K], M.Builder[(K, A), F[K, A]])](Set.empty[K], cbf()) {
      case ((ks, builder), a) ⇒ f(a).calc(k ⇒ (!ks.contains(k)).option(ks + k, builder += ((k, a))))
    }.map { case (_, builder) ⇒ builder.result() }
  }
}

case class UngroupBy[A, B, CC[_]](ungrouped: Map[Int, M.Builder[A, CC[A]]], counts: Map[B, Int])(
  implicit inner: CCBF[A, CC], outer: CCBF[CC[A], CC]) {

  def add(a: A, b: B): UngroupBy[A, B, CC] = copy(ungrouped + entry(count(b), a), counts + ((b, count(b))))
  def values: CC[CC[A]] = ungrouped.sorted.values.map(_.result())(breakOut(outer))

  private def entry(count: Int, a: A) = (count, ungrouped.getOrElse(count, inner.apply()) += a)
  private def count(b: B) = counts.getOrElse(b, 0) + 1
}
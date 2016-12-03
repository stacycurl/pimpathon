package pimpathon

import scala.language.{higherKinds, implicitConversions, reflectiveCalls}

import scala.{PartialFunction ⇒ ~>}
import scala.annotation.tailrec
import scala.collection.{breakOut, mutable ⇒ M, GenTraversable, GenTraversableLike}
import scala.collection.immutable.{Map ⇒ ▶:}
import scala.collection.generic.CanBuildFrom

import pimpathon.any._
import pimpathon.boolean._
import pimpathon.builder._
import pimpathon.either._
import pimpathon.function._
import pimpathon.map._
import pimpathon.multiMap._
import pimpathon.option._
import pimpathon.tuple._


object genTraversableLike {
  type GTLGT[+A] = GenTraversableLike[A, GenTraversable[A]]

  trait GenTraversableLikePimpsMixin[A, CC[_]] {
    protected def cc: CC[A]
    protected def gtl: GTLGT[A]

    def asMap: GenTraversableLikeCapturer[A, ▶:, ▶:] = as[▶:]

    object histogram extends aggregator[Lambda[(X, Y) => X ▶: Int]] {
      protected implicit def cbf[B]: CanBuildFrom[Nothing, (B, A), B ▶: Int] =
        multiMap.build[Lambda[X ⇒ Int], B, A](CanBuildNonEmpty.canBuildFromToCBNE(CountCBF))
    }

    object detect extends aggregator[Lambda[(X, Y) ⇒ Set[X]]] {
      protected implicit def cbf[B]: CanBuildFrom[Nothing, (B, A), Set[B]] =
        Set.canBuildFrom[B].on[(B, A)] { case (b, _) ⇒ b }
    }

    abstract class aggregator[▷:[_, _]] {
      def apply(): A ▷: A = by(id)


      def by[B](b: A ⇒ B): B ▷: A =
        counter.withEntries(b, id)

      def by[C, B](c: A ⇒ C, b: A ⇒ B): C ▶: B ▷: A =
        counter.withEntries(c, b, id)

      def by[D, C, B](d: A ⇒ D, c: A ⇒ C, b: A ⇒ B): D ▶: C ▶: B ▷: A =
        counter.withEntries(d, c, b, id)

      def by[E, D, C, B](e: A ⇒ E, d: A ⇒ D, c: A ⇒ C, b: A ⇒ B): E ▶: D ▶: C ▶: B ▷: A =
        counter.withEntries(e, d, c, b, id)

      def by[F, E, D, C, B](f: A ⇒ F, e: A ⇒ E, d: A ⇒ D, c: A ⇒ C, b: A ⇒ B): F ▶: E ▶: D ▶: C ▶: B ▷: A =
        counter.withEntries(f, e, d, c, b, id)


      def collect[B](b: A ~> B): B ▷: A =
        counter.withPFEntries(b, idp)

      def collect[C, B](c: A ~> C, b: A ~> B): C ▶: B ▷: A =
        counter.withPFEntries(c, b, idp)

      def collect[D, C, B](d: A ~> D, c: A ~> C, b: A ~> B): D ▶: C ▶: B ▷: A =
        counter.withPFEntries(d, c, b, idp)

      def collect[E, D, C, B](e: A ~> E, d: A ~> D, c: A ~> C, b: A ~> B): E ▶: D ▶: C ▶: B ▷: A =
        counter.withPFEntries(e, d, c, b, idp)

      def collect[F, E, D, C, B](f: A ~> F, e: A ~> E, d: A ~> D, c: A ~> C, b: A ~> B): F ▶: E ▶: D ▶: C ▶: B ▷: A =
        counter.withPFEntries(f, e, d, c, b, idp)


      def opt[B](b: A ⇒ Option[B]): B ▷: A =
        counter.withSomeEntries(b, Some(_))

      def opt[C, B](c: A ⇒ Option[C], b: A ⇒ Option[B]): C ▶: B ▷: A =
        counter.withSomeEntries(c, b, Some(_))

      def opt[D, C, B](d: A ⇒ Option[D], c: A ⇒ Option[C], b: A ⇒ Option[B]): D ▶: C ▶: B ▷: A =
        counter.withSomeEntries(d, c, b, Some(_))

      def opt[E, D, C, B](e: A ⇒ Option[E], d: A ⇒ Option[D], c: A ⇒ Option[C], b: A ⇒ Option[B]): E ▶: D ▶: C ▶: B ▷: A =
        counter.withSomeEntries(e, d, c, b, Some(_))

      def opt[F, E, D, C, B](f: A ⇒ Option[F], e: A ⇒ Option[E], d: A ⇒ Option[D], c: A ⇒ Option[C], b: A ⇒ Option[B]): F ▶: E ▶: D ▶: C ▶: B ▷: A =
        counter.withSomeEntries(f, e, d, c, b, Some(_))


      def many[B](b: A ⇒ GTLGT[B]): B ▷: A =
        counter.withManyEntries(b, List(_))

      def many[C, B](c: A ⇒ GTLGT[C], b: A ⇒ GTLGT[B]): C ▶: B ▷: A =
        counter.withManyEntries(c, b, List(_))

      def many[D, C, B](d: A ⇒ GTLGT[D], c: A ⇒ GTLGT[C], b: A ⇒ GTLGT[B]): D ▶: C ▶: B ▷: A =
        counter.withManyEntries(d, c, b, List(_))

      def many[E, D, C, B](e: A ⇒ GTLGT[E], d: A ⇒ GTLGT[D], c: A ⇒ GTLGT[C], b: A ⇒ GTLGT[B]): E ▶: D ▶: C ▶: B ▷: A =
        counter.withManyEntries(e, d, c, b, List(_))

      def many[F, E, D, C, B](f: A ⇒ GTLGT[F], e: A ⇒ GTLGT[E], d: A ⇒ GTLGT[D], c: A ⇒ GTLGT[C], b: A ⇒ GTLGT[B]): F ▶: E ▶: D ▶: C ▶: B ▷: A =
        counter.withManyEntries(f, e, d, c, b, List(_))

      protected implicit def cbf[B]: CanBuildFrom[Nothing, (B, A), B ▷: A]

      private def counter: GenTraversableLikeCapturer[A, ▶:, ▷:] = using[▶:, ▷:]
      private val (id, idp) = (identity[A] _, identityPF[A])
    }

    def asMultiMap[F[_]]: GenTraversableLikeCapturer[A, ▶:, MultiMap[F, ?, ?]] = using[▶:, MultiMap[F, ?, ?]]

    def as[F[_, _]]: GenTraversableLikeCapturer[A, F, F] = using[F, F]

    def using[F[_, _], G[_, _]]: GenTraversableLikeCapturer[A, F, G] = GenTraversableLikeCapturer[A, F, G](gtl)

    def ungroupBy[B](f: A ⇒ B)(implicit inner: CCBF[A, CC], outer: CCBF[CC[A], CC]): CC[CC[A]] =
      gtl.foldLeft(UngroupBy[A, B, CC](Map(), Map())) { case (ungroupBy, item) ⇒ ungroupBy.add(item, f(item)) }.values

    def partitionByPF[B](pf: A ~> B)
      (implicit eab: CCBF[Either[A, B], CC], a: CCBF[A, CC], b: CCBF[B, CC]): (CC[A], CC[B]) = pf.partition[CC](gtl)

    def none(a: A): Boolean = gtl.forall(_ != a)
    def all(a: A): Boolean  = gtl.forall(_ == a)

    def onlyOrThrow(f: CC[A] ⇒ Exception): A = onlyOption.getOrThrow(f(cc))
    def onlyEither: Either[CC[A], A] = onlyOrEither(cc ⇒ cc)
    def onlyOrEither[B](f: CC[A] ⇒ B): Either[B, A] = onlyOption.toRight(f(cc))
    def onlyOption: Option[A] = if (gtl.isEmpty || gtl.tail.nonEmpty) None else gtl.headOption

    def seqMap[B, To](f: A ⇒ Option[B])(implicit cbf: CanBuildFrom[Nothing, B, To]): Option[To] =
      seqFold[M.Builder[B, To]](cbf())((builder, a) ⇒ f(a).map(builder += _)).map(_.result())

    def seqFold[B](z: B)(op: (B, A) ⇒ Option[B]): Option[B] = // similar to scalaz' GTL.foldLeftM[Option, B, A]
      apoFold[B, B](z)((b, a) ⇒ op(b, a).toRight(b)).right.toOption

    def apoFold[B, C](z: B)(op: (B, A) ⇒ Either[C, B]): Either[C, B] = {
      @tailrec def recurse(cur: GTLGT[A], acc: B): Either[C, B] = cur.headOption match {
        case None ⇒ Right(acc)
        case Some(a) ⇒ op(acc, a) match {
          case Right(b) ⇒ recurse(cur.tail, b)
          case done     ⇒ done
        }
      }

      recurse(gtl, z)
    }
  }

  trait GenTraversableLikeOfEitherPimpsMixin[L, R, CC[_]] {
    def partitionEithers[That[_]](implicit lcbf: CCBF[L, That], rcbf: CCBF[R, That]): (That[L], That[R]) =
      (lcbf.apply(), rcbf.apply()).tap(l ⇒ r ⇒ gtl.foreach(_.addTo(l, r))).tmap(_.result(), _.result())

    protected def gtl: GTLGT[Either[L, R]]
  }

  trait GenTraversableLikeOfTuple2Mixin[K, V] {
    def toMultiMap[F[_]](implicit fcbf: CanBuildNonEmpty[V, F[V]]): K ▶: F[V] = gtl.map(kv ⇒ kv)(breakOut)

    protected def gtl: GTLGT[(K, V)]
  }

  implicit class GenTraversableLikePimps[A](self: GTLGT[A]) extends GenTraversableLikePimpsMixin[A, GTLGT] {
    protected def cc: GTLGT[A]  = self
    protected def gtl: GTLGT[A] = self
  }

  implicit class GenTraversableLikeOfEitherPimps[L, R](self: GTLGT[Either[L, R]]) extends GenTraversableLikeOfEitherPimpsMixin[L, R, GTLGT] {
    protected def gtl: GTLGT[Either[L, R]] = self
  }

  implicit class GenTraversableLikeOfTuple2[K, V](self: GTLGT[(K, V)]) extends GenTraversableLikeOfTuple2Mixin[K, V] {
    protected def gtl: GTLGT[(K, V)] = self
  }
}

case class GenTraversableLikeCapturer[A, ⪢:[_, _], ▷:[_, _]](private val gtl: GenTraversableLike[A, GenTraversable[A]]) {
  import pimpathon.genTraversableLike._
  type CBF[K, V] = CanBuildFrom[Nothing, (K, V), K ▷: V]
  type GBF[K, V] = CanBuildFrom[Nothing, (K, V), K ⪢: V]

  def withKeys[K](f: A ⇒ K)(implicit cbf: CBF[K, A]):    K ▷: A = withEntries(a ⇒ (f(a), a))
  def withValues[V](f: A ⇒ V)(implicit cbf: CBF[A, V]):  A ▷: V = withEntries(a ⇒ (a, f(a)))
  def withConstValue[V](v: V)(implicit  cbf: CBF[A, V]): A ▷: V = withEntries(a ⇒ (a, v))

  def withEntries[K5, K4, K3, K2, K1, V](f5: A ⇒ K5, f4: A ⇒ K4, f3: A ⇒ K3, f2: A ⇒ K2, f1: A ⇒ K1, fv: A ⇒ V)(implicit
    k1: CBF[K1, V],
    k2: GBF[K2, K1 ▷: V],
    k3: GBF[K3, K2 ⪢: K1 ▷: V],
    k4: GBF[K4, K3 ⪢: K2 ⪢: K1 ▷: V],
    k5: GBF[K5, K4 ⪢: K3 ⪢: K2 ⪢: K1 ▷: V]
  ): K5 ⪢: K4 ⪢: K3 ⪢: K2 ⪢: K1 ▷: V = groupBy(f5, _.withEntries(f4, f3, f2, f1, fv))

  def withEntries[K4, K3, K2, K1, V](f4: A ⇒ K4, f3: A ⇒ K3, f2: A ⇒ K2, f1: A ⇒ K1, fv: A ⇒ V)(implicit
    k1: CBF[K1, V],
    k2: GBF[K2, K1 ▷: V],
    k3: GBF[K3, K2 ⪢: K1 ▷: V],
    k4: GBF[K4, K3 ⪢: K2 ⪢: K1 ▷: V]
  ): K4 ⪢: K3 ⪢: K2 ⪢: K1 ▷: V = groupBy(f4, _.withEntries(f3, f2, f1, fv))

  def withEntries[K3, K2, K1, V](f3: A ⇒ K3, f2: A ⇒ K2, f1: A ⇒ K1, fv: A ⇒ V)(implicit
    k1: CBF[K1, V],
    k2: GBF[K2, K1 ▷: V],
    k3: GBF[K3, K2 ⪢: K1 ▷: V]
  ): K3 ⪢: K2 ⪢: K1 ▷: V = groupBy(f3, _.withEntries(f2, f1, fv))

  def withEntries[K2, K1, V](f2: A ⇒ K2, f1: A ⇒ K1, fv: A ⇒ V)(implicit
    k1: CBF[K1, V],
    k2: GBF[K2, K1 ▷: V]
  ): K2 ⪢: K1 ▷: V = groupBy(f2, _.withEntries(f1, fv))

  def withEntries[K, V](k: A ⇒ K, v: A ⇒ V)(implicit cbf: CBF[K, V]): K ▷: V = gtl.map(a ⇒ (k(a), v(a)))(breakOut)
  def withEntries[K, V](f: A ⇒ ((K, V)))(implicit cbf: CBF[K, V]):    K ▷: V = gtl.map(f)(breakOut)

  def withSomeKeys[K](f: A ⇒ Option[K])(implicit cbf: CBF[K, A]):   K ▷: A = withSomeEntries(a ⇒ f(a).map(_ → a))
  def withSomeValues[V](f: A ⇒ Option[V])(implicit cbf: CBF[A, V]): A ▷: V = withSomeEntries(a ⇒ f(a).map(a → _))


  def withSomeEntries[K5, K4, K3, K2, K1, V](
    f5: A ⇒ Option[K5], f4: A ⇒ Option[K4], f3: A ⇒ Option[K3], f2: A ⇒ Option[K2], f1: A ⇒ Option[K1], fv: A ⇒ Option[V])(implicit
    k1: CBF[K1, V],
    k2: GBF[K2, K1 ▷: V],
    k3: GBF[K3, K2 ⪢: K1 ▷: V],
    k4: GBF[K4, K3 ⪢: K2 ⪢: K1 ▷: V],
    k5: GBF[K5, K4 ⪢: K3 ⪢: K2 ⪢: K1 ▷: V]
  ): K5 ⪢: K4 ⪢: K3 ⪢: K2 ⪢: K1 ▷: V = optGroupBy(f5, _.withSomeEntries(f4, f3, f2, f1, fv))

  def withSomeEntries[K4, K3, K2, K1, V](
    f4: A ⇒ Option[K4], f3: A ⇒ Option[K3], f2: A ⇒ Option[K2], f1: A ⇒ Option[K1], fv: A ⇒ Option[V])(implicit
    k1: CBF[K1, V],
    k2: GBF[K2, K1 ▷: V],
    k3: GBF[K3, K2 ⪢: K1 ▷: V],
    k4: GBF[K4, K3 ⪢: K2 ⪢: K1 ▷: V]
  ): K4 ⪢: K3 ⪢: K2 ⪢: K1 ▷: V = optGroupBy(f4, _.withSomeEntries(f3, f2, f1, fv))

  def withSomeEntries[K3, K2, K1, V](f3: A ⇒ Option[K3], f2: A ⇒ Option[K2], f1: A ⇒ Option[K1], fv: A ⇒ Option[V])(implicit
    k1: CBF[K1, V],
    k2: GBF[K2, K1 ▷: V],
    k3: GBF[K3, K2 ⪢: K1 ▷: V]
  ): K3 ⪢: K2 ⪢: K1 ▷: V = optGroupBy(f3, _.withSomeEntries(f2, f1, fv))

  def withSomeEntries[K2, K1, V](f2: A ⇒ Option[K2], f1: A ⇒ Option[K1], fv: A ⇒ Option[V])(implicit
    k1: CBF[K1, V],
    k2: GBF[K2, K1 ▷: V]
  ): K2 ⪢: K1 ▷: V = optGroupBy(f2, _.withSomeEntries(f1, fv))

  def withSomeEntries[K, V](fk: A ⇒ Option[K], fv: A ⇒ Option[V])(implicit cbf: CBF[K, V]): K ▷: V = withSomeEntries(zip(fk, fv))

  def withSomeEntries[K, V](f: A ⇒ Option[(K, V)])(implicit cbf: CBF[K, V]): K ▷: V = gtl.flatMap(a ⇒ f(a))(breakOut)

  def withPFKeys[K](pf: A ~> K)(implicit cbf: CBF[K, A]):   K ▷: A = withPFEntries(pf &&& identityPF[A])
  def withPFValues[V](pf: A ~> V)(implicit cbf: CBF[A, V]): A ▷: V = withPFEntries(identityPF[A] &&& pf)

  def withPFEntries[K5, K4, K3, K2, K1, V](f5: A ~> K5, f4: A ~> K4, f3: A ~> K3, f2: A ~> K2, f1: A ~> K1, fv: A ~> V)(implicit
    k1: CBF[K1, V],
    k2: GBF[K2, K1 ▷: V],
    k3: GBF[K3, K2 ⪢: K1 ▷: V],
    k4: GBF[K4, K3 ⪢: K2 ⪢: K1 ▷: V],
    k5: GBF[K5, K4 ⪢: K3 ⪢: K2 ⪢: K1 ▷: V]
  ): K5 ⪢: K4 ⪢: K3 ⪢: K2 ⪢: K1 ▷: V = pfGroupBy(f5, _.withPFEntries(f4, f3, f2, f1, fv))

  def withPFEntries[K4, K3, K2, K1, V](f4: A ~> K4, f3: A ~> K3, f2: A ~> K2, f1: A ~> K1, fv: A ~> V)(implicit
    k1: CBF[K1, V],
    k2: GBF[K2, K1 ▷: V],
    k3: GBF[K3, K2 ⪢: K1 ▷: V],
    k4: GBF[K4, K3 ⪢: K2 ⪢: K1 ▷: V]
  ): K4 ⪢: K3 ⪢: K2 ⪢: K1 ▷: V = pfGroupBy(f4, _.withPFEntries(f3, f2, f1, fv))

  def withPFEntries[K3, K2, K1, V](f3: A ~> K3, f2: A ~> K2, f1: A ~> K1, fv: A ~> V)(implicit
    k1: CBF[K1, V],
    k2: GBF[K2, K1 ▷: V],
    k3: GBF[K3, K2 ⪢: K1 ▷: V]
  ): K3 ⪢: K2 ⪢: K1 ▷: V = pfGroupBy(f3, _.withPFEntries(f2, f1, fv))

  def withPFEntries[K2, K1, V](f2: A ~> K2, f1: A ~> K1, fv: A ~> V)(implicit
    k1: CBF[K1, V],
    k2: GBF[K2, K1 ▷: V]
  ): K2 ⪢: K1 ▷: V = pfGroupBy(f2, _.withPFEntries(f1, fv))

  def withPFEntries[K, V](k: A ~> K, v: A ~> V)(implicit cbf: CBF[K, V]): K ▷: V = withPFEntries(k &&& v)


  def withPFEntries[K, V](pf: A ~> (K, V))(implicit cbf: CBF[K, V]): K ▷: V = gtl.collect(pf)(breakOut)

  def withManyEntries[K5, K4, K3, K2, K1, V](f5: A ⇒ GTLGT[K5], f4: A ⇒ GTLGT[K4], f3: A ⇒ GTLGT[K3], f2: A ⇒ GTLGT[K2], f1: A ⇒ GTLGT[K1], fv: A ⇒ GTLGT[V])(implicit
    k1: CBF[K1, V],
    k2: GBF[K2, K1 ▷: V],
    k3: GBF[K3, K2 ⪢: K1 ▷: V],
    k4: GBF[K4, K3 ⪢: K2 ⪢: K1 ▷: V],
    k5: GBF[K5, K4 ⪢: K3 ⪢: K2 ⪢: K1 ▷: V]
  ): K5 ⪢: K4 ⪢: K3 ⪢: K2 ⪢: K1 ▷: V = manyGroupBy(f5, _.withManyEntries(f4, f3, f2, f1, fv))

  def withManyEntries[K4, K3, K2, K1, V](f4: A ⇒ GTLGT[K4], f3: A ⇒ GTLGT[K3], f2: A ⇒ GTLGT[K2], f1: A ⇒ GTLGT[K1], fv: A ⇒ GTLGT[V])(implicit
    k1: CBF[K1, V],
    k2: GBF[K2, K1 ▷: V],
    k3: GBF[K3, K2 ⪢: K1 ▷: V],
    k4: GBF[K4, K3 ⪢: K2 ⪢: K1 ▷: V]
  ): K4 ⪢: K3 ⪢: K2 ⪢: K1 ▷: V = manyGroupBy(f4, _.withManyEntries(f3, f2, f1, fv))

  def withManyEntries[K3, K2, K1, V](f3: A ⇒ GTLGT[K3], f2: A ⇒ GTLGT[K2], f1: A ⇒ GTLGT[K1], fv: A ⇒ GTLGT[V])(implicit
    k1: CBF[K1, V],
    k2: GBF[K2, K1 ▷: V],
    k3: GBF[K3, K2 ⪢: K1 ▷: V]
  ): K3 ⪢: K2 ⪢: K1 ▷: V = manyGroupBy(f3, _.withManyEntries(f2, f1, fv))

  def withManyEntries[K2, K1, V](f2: A ⇒ GTLGT[K2], f1: A ⇒ GTLGT[K1], fv: A ⇒ GTLGT[V])(implicit
    k1: CBF[K1, V],
    k2: GBF[K2, K1 ▷: V]
  ): K2 ⪢: K1 ▷: V = manyGroupBy(f2, _.withManyEntries(f1, fv))

  def withManyEntries[K, V](fk: A ⇒ GTLGT[K], fv: A ⇒ GTLGT[V])(implicit cbf: CBF[K, V]): K ▷: V =
    (for { a ← gtl; k ← fk(a); v ← fv(a) } yield (k, v))(breakOut)

  def withManyKeys[K](f: A ⇒ GTLGT[K])(implicit cbf: CBF[K, A]): K ▷: A =
    gtl.flatMap(a ⇒ f(a).map(_ → a))(breakOut)

  def withUniqueKeys[K](f: A ⇒ K)(implicit cbf: CBF[K, A]): Option[K ▷: A] = {
    gtl.seqFold[(Set[K], M.Builder[(K, A), K ▷: A])](Set.empty[K], cbf()) {
      case ((ks, builder), a) ⇒ f(a).calc(k ⇒ (!ks.contains(k)).option(ks + k, builder += ((k, a))))
    }.map { case (_, builder) ⇒ builder.result() }
  }

  private type Self[X, Y[_, _], Z[_, _]] = GenTraversableLikeCapturer[X, Y, Z]

  private def groupBy[K, V](fk: A ⇒ K, grouping: Self[A, ⪢:, ▷:] ⇒ V)(implicit cbf: GBF[K, V]): K ⪢: V =
    gtl.asMultiMap[List].withKeys(fk).calc(next(grouping))

  private def optGroupBy[K, V](fk: A ⇒ Option[K], grouping: Self[A, ⪢:, ▷:] ⇒ V)(implicit cbf: GBF[K, V]): K ⪢: V =
    gtl.asMultiMap[List].withSomeKeys(fk).calc(next(grouping))

  private def manyGroupBy[K, V](fk: A ⇒ GTLGT[K], grouping: Self[A, ⪢:, ▷:] ⇒ V)(implicit cbf: GBF[K, V]): K ⪢: V =
    gtl.asMultiMap[List].withManyKeys(fk).calc(next(grouping))

  private def pfGroupBy[K, V](fk: A ~> K, grouping: Self[A, ⪢:, ▷:] ⇒ V)(implicit cbf: GBF[K, V]): K ⪢: V =
    gtl.asMultiMap[List].withPFKeys(fk).calc(next(grouping))

  private def next[V, K](f: Self[A, ⪢:, ▷:] ⇒ V)(implicit cbf: GBF[K, V]): K ▶: List[A] ⇒ K ⪢: V =
    inner ⇒ inner.map { case (k, as) ⇒ (k, f(copy(as))) }(breakOut)

  private def zip[K, V](fk: A ⇒ Option[K], fv: A ⇒ Option[V])(a: A): Option[(K, V)] = for { k ← fk(a); v ← fv(a) } yield (k, v)
}

case class UngroupBy[A, B, CC[_]](ungrouped: Int ▶: M.Builder[A, CC[A]], counts: B ▶: Int)(
  implicit inner: CCBF[A, CC], outer: CCBF[CC[A], CC]) {

  def add(a: A, b: B): UngroupBy[A, B, CC] = copy(ungrouped + entry(count(b), a), counts + ((b, count(b))))
  def values: CC[CC[A]] = ungrouped.sorted.values.map(_.result())(breakOut(outer))

  private def entry(count: Int, a: A) = (count, ungrouped.getOrElse(count, inner.apply()) += a)
  private def count(b: B) = counts.getOrElse(b, 0) + 1
}

object CountCBF extends CanBuildFrom[Nothing, Any, Int] with IgnoreFromCBF[Nothing, Any, Int] {
  def apply(): M.Builder[Any, Int] = new M.Builder[Any, Int] {
    def +=(elem: Any): this.type = { count = count + 1; this }
    def result(): Int = count
    def clear(): Unit = { count = 0 }

    private var count = 0
  }
}
package pimpathon

import scala.language.{higherKinds, implicitConversions, reflectiveCalls}

import scala.{PartialFunction ⇒ ~>}
import scala.annotation.tailrec
import scala.collection.{breakOut, mutable ⇒ M, GenTraversable, GenTraversableLike}
import scala.collection.generic.CanBuildFrom

import pimpathon.any._
import pimpathon.boolean._
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

    def asMap: GenTraversableLikeCapturer[A, Map, Map] = as[Map]

    case object histogram {
      def apply(): Map[A, Int] = by(a ⇒ a)


      def by[B](f: A ⇒ B): Map[B, Int] =
        asMultiMap[Count].withKeys(f)

      def by[B, C](f: A ⇒ B, g: A ⇒ C): Map[B, Map[C, Int]] =
        asMultiMap[Count].withEntries(f, g, a ⇒ a)

      def by[B, C, D](f: A ⇒ B, g: A ⇒ C, h: A ⇒ D): Map[B, Map[C, Map[D, Int]]] =
        asMultiMap[Count].withEntries(f, g, h, a ⇒ a)

      def by[B, C, D, E](f: A ⇒ B, g: A ⇒ C, h: A ⇒ D, i: A ⇒ E): Map[B, Map[C, Map[D, Map[E, Int]]]] =
        asMultiMap[Count].withEntries(f, g, h, i, a ⇒ a)


      def collect[B](f: A ~> B): Map[B, Int] =
        asMultiMap[Count].withPFKeys(f)

      def collect[B, C](f: A ~> B, g: A ~> C): Map[B, Map[C, Int]] =
        asMultiMap[Count].withPFEntries(f, g, ~>(a ⇒ a))

      def collect[B, C, D](f: A ~> B, g: A ~> C, h: A ~> D): Map[B, Map[C, Map[D, Int]]] =
        asMultiMap[Count].withPFEntries(f, g, h, ~>(a ⇒ a))

      def collect[B, C, D, E](f: A ~> B, g: A ~> C, h: A ~> D, i: A ~> E): Map[B, Map[C, Map[D, Map[E, Int]]]] =
        asMultiMap[Count].withPFEntries(f, g, h, i, ~>(a ⇒ a))


      def opt[B](f: A ⇒ Option[B]): Map[B, Int] =
        asMultiMap[Count].withSomeKeys(f)

      def opt[B, C](f: A ⇒ Option[B], g: A ⇒ Option[C]): Map[B, Map[C, Int]] =
        asMultiMap[Count].withSomeEntries(f, g, a ⇒ Some(a))

      def opt[B, C, D](f: A ⇒ Option[B], g: A ⇒ Option[C], h: A ⇒ Option[D]): Map[B, Map[C, Map[D, Int]]] =
        asMultiMap[Count].withSomeEntries(f, g, h, a ⇒ Some(a))

      def opt[B, C, D, E](f: A ⇒ Option[B], g: A ⇒ Option[C], h: A ⇒ Option[D], i: A ⇒ Option[E]): Map[B, Map[C, Map[D, Map[E, Int]]]] =
        asMultiMap[Count].withSomeEntries(f, g, h, i, a ⇒ Some(a))
    }

    def asMultiMap[F[_]]: GenTraversableLikeCapturer[A, ({ type MM[K, V] = MultiMap[F, K, V] })#MM, Map] =
      GenTraversableLikeCapturer[A, ({ type MM[K, V] = MultiMap[F, K, V] })#MM, Map](gtl)

    def as[F[_, _]]: GenTraversableLikeCapturer[A, F, F] = GenTraversableLikeCapturer[A, F, F](gtl)

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

    private implicit def countValues[B]: CanBuildFrom[Nothing, (B, A), Map[B, Int]] =
      multiMap.build[Count, B, A](CanBuildNonEmpty.canBuildFromToCBNE(CountCBF))

    private type Count[_] = Int
  }

  trait GenTraversableLikeOfEitherPimpsMixin[L, R, CC[_]] {
    def partitionEithers[That[_]](implicit lcbf: CCBF[L, That], rcbf: CCBF[R, That]): (That[L], That[R]) =
      (lcbf.apply(), rcbf.apply()).tap(l ⇒ r ⇒ gtl.foreach(_.addTo(l, r))).tmap(_.result(), _.result())

    protected def gtl: GTLGT[Either[L, R]]
  }

  trait GenTraversableLikeOfTuple2Mixin[K, V] {
    def toMultiMap[F[_]](implicit fcbf: CanBuildNonEmpty[V, F[V]]): MultiMap[F, K, V] = gtl.map(kv ⇒ kv)(breakOut)

    protected def gtl: GTLGT[(K, V)]
  }

  implicit class GenTraversableLikePimps[A](protected val gtl: GTLGT[A])
    extends GenTraversableLikePimpsMixin[A, GTLGT] {

    protected def cc: GTLGT[A] = gtl
  }

  implicit class GenTraversableLikeOfEitherPimps[L, R](protected val gtl: GTLGT[Either[L, R]])
    extends GenTraversableLikeOfEitherPimpsMixin[L, R, GTLGT]

  implicit class GenTraversableLikeOfTuple2[K, V](protected val gtl: GTLGT[(K, V)])
    extends GenTraversableLikeOfTuple2Mixin[K, V]
}


case class GenTraversableLikeCapturer[A, F[_, _], G[_, _]](private val gtl: GenTraversableLike[A, GenTraversable[A]]) {
  import pimpathon.genTraversableLike._
  type CBF[K, V] = CanBuildFrom[Nothing, (K, V), F[K, V]]
  type GBF[K, V] = CanBuildFrom[Nothing, (K, V), G[K, V]]

  def withKeys[K](f: A ⇒ K)(implicit cbf: CBF[K, A]): F[K, A]    = withEntries(a ⇒ (f(a), a))
  def withValues[V](f: A ⇒ V)(implicit cbf: CBF[A, V]): F[A, V]  = withEntries(a ⇒ (a, f(a)))
  def withConstValue[V](v: V)(implicit  cbf: CBF[A, V]): F[A, V] = withEntries(a ⇒ (a, v))


  def withEntries[K1, K2, K3, K4, V](fk1: A ⇒ K1, fk2: A ⇒ K2, fk3: A ⇒ K3, fk4: A ⇒ K4, fv: A ⇒ V)(implicit
    k4: CBF[K4, V], k3: GBF[K3, F[K4, V]], k2: GBF[K2, G[K3, F[K4, V]]], k1: GBF[K1, G[K2, G[K3, F[K4, V]]]]
  ): G[K1, G[K2, G[K3, F[K4, V]]]] = groupBy(fk1, _.withEntries(fk2, fk3, fk4, fv))

  def withEntries[K1, K2, K3, V](fk1: A ⇒ K1, fk2: A ⇒ K2, fk3: A ⇒ K3, fv: A ⇒ V)(implicit
    k3: CBF[K3, V], k2: GBF[K2, F[K3, V]], k1: GBF[K1, G[K2, F[K3, V]]]
  ): G[K1, G[K2, F[K3, V]]] = groupBy(fk1, _.withEntries(fk2, fk3, fv))

  def withEntries[K1, K2, V](fk1: A ⇒ K1, fk2: A ⇒ K2, fv: A ⇒ V)(implicit
    k2: CBF[K2, V], k1: GBF[K1, F[K2, V]]
  ): G[K1, F[K2, V]] = groupBy(fk1, _.withEntries(fk2, fv))

  def withEntries[K, V](k: A ⇒ K, v: A ⇒ V)(implicit cbf: CBF[K, V]): F[K, V] = gtl.map(a ⇒ (k(a), v(a)))(breakOut)
  def withEntries[K, V](f: A ⇒ ((K, V)))(implicit cbf: CBF[K, V]): F[K, V] = gtl.map(f)(breakOut)

  def withSomeKeys[K](f: A ⇒ Option[K])(implicit cbf: CBF[K, A]): F[K, A]   = withSomeEntries(a ⇒ f(a).map(_ → a))
  def withSomeValues[V](f: A ⇒ Option[V])(implicit cbf: CBF[A, V]): F[A, V] = withSomeEntries(a ⇒ f(a).map(a → _))


  def withSomeEntries[K1, K2, K3, K4, V](fk1: A ⇒ Option[K1], fk2: A ⇒ Option[K2], fk3: A ⇒ Option[K3], fk4: A ⇒ Option[K4], fv: A ⇒ Option[V])(implicit
    k4: CBF[K4, V], k3: GBF[K3, F[K4, V]], k2: GBF[K2, G[K3, F[K4, V]]], k1: GBF[K1, G[K2, G[K3, F[K4, V]]]]
  ): G[K1, G[K2, G[K3, F[K4, V]]]] = optGroupBy(fk1, _.withSomeEntries(fk2, fk3, fk4, fv))

  def withSomeEntries[K1, K2, K3, V](fk1: A ⇒ Option[K1], fk2: A ⇒ Option[K2], fk3: A ⇒ Option[K3], fv: A ⇒ Option[V])(implicit
    k3: CBF[K3, V], k2: GBF[K2, F[K3, V]], k1: GBF[K1, G[K2, F[K3, V]]]
  ): G[K1, G[K2, F[K3, V]]] = optGroupBy(fk1, _.withSomeEntries(fk2, fk3, fv))

  def withSomeEntries[K1, K2, V](fk1: A ⇒ Option[K1], fk2: A ⇒ Option[K2], fv: A ⇒ Option[V])(implicit
    k2: CBF[K2, V], k1: GBF[K1, F[K2, V]]
  ): G[K1, F[K2, V]] = optGroupBy(fk1, _.withSomeEntries(fk2, fv))

  def withSomeEntries[K, V](fk: A ⇒ Option[K], fv: A ⇒ Option[V])(implicit cbf: CBF[K, V]): F[K, V] = withSomeEntries(zip(fk, fv))

  def withSomeEntries[K, V](f: A ⇒ Option[(K, V)])(implicit cbf: CBF[K, V]): F[K, V] = gtl.flatMap(a ⇒ f(a))(breakOut)

  def withPFKeys[K](pf: A ~> K)(implicit cbf: CBF[K, A]): F[K, A]   = withPFEntries(pf &&& identityPF[A])
  def withPFValues[V](pf: A ~> V)(implicit cbf: CBF[A, V]): F[A, V] = withPFEntries(identityPF[A] &&& pf)

  def withPFEntries[K1, K2, K3, K4, V](fk1: A ~> K1, fk2: A ~> K2, fk3: A ~> K3, fk4: A ~> K4, fv: A ~> V)(implicit
    k4: CBF[K4, V], k3: GBF[K3, F[K4, V]], k2: GBF[K2, G[K3, F[K4, V]]], k1: GBF[K1, G[K2, G[K3, F[K4, V]]]]
  ): G[K1, G[K2, G[K3, F[K4, V]]]] = pfGroupBy(fk1, _.withPFEntries(fk2, fk3, fk4, fv))

  def withPFEntries[K1, K2, K3, V](fk1: A ~> K1, fk2: A ~> K2, fk3: A ~> K3, fv: A ~> V)(implicit
    k3: CBF[K3, V], k2: GBF[K2, F[K3, V]], k1: GBF[K1, G[K2, F[K3, V]]]
  ): G[K1, G[K2, F[K3, V]]] = pfGroupBy(fk1, _.withPFEntries(fk2, fk3, fv))

  def withPFEntries[K1, K2, V](fk1: A ~> K1, fk2: A ~> K2, fv: A ~> V)(implicit
    k2: CBF[K2, V], k1: GBF[K1, F[K2, V]]
  ): G[K1, F[K2, V]] = pfGroupBy(fk1, _.withPFEntries(fk2, fv))

  def withPFEntries[K, V](k: A ~> K, v: A ~> V)(implicit cbf: CBF[K, V]): F[K, V] = withPFEntries(k &&& v)


  def withPFEntries[K, V](pf: A ~> (K, V))(implicit cbf: CBF[K, V]): F[K, V] = gtl.collect(pf)(breakOut)

  def withManyKeys[K](f: A ⇒ List[K])(implicit cbf: CBF[K, A]): F[K, A] =
    gtl.flatMap(a ⇒ f(a).map(_ → a))(breakOut)

  def withUniqueKeys[K](f: A ⇒ K)(implicit cbf: CBF[K, A]): Option[F[K, A]] = {
    gtl.seqFold[(Set[K], M.Builder[(K, A), F[K, A]])](Set.empty[K], cbf()) {
      case ((ks, builder), a) ⇒ f(a).calc(k ⇒ (!ks.contains(k)).option(ks + k, builder += ((k, a))))
    }.map { case (_, builder) ⇒ builder.result() }
  }

  private type Self[X, Y[_, _], Z[_, _]] = GenTraversableLikeCapturer[X, Y, Z]

  private def groupBy[K, V](fk: A ⇒ K, grouping: Self[A, F, G] ⇒ V)(implicit cbf: GBF[K, V]): G[K, V] =
    gtl.asMultiMap[List].withKeys(fk).calc(next(grouping))

  private def optGroupBy[K, V](fk: A ⇒ Option[K], grouping: Self[A, F, G] ⇒ V)(implicit cbf: GBF[K, V]): G[K, V] =
    gtl.asMultiMap[List].withSomeKeys(fk).calc(next(grouping))

  private def pfGroupBy[K, V](fk: A ~> K, grouping: Self[A, F, G] ⇒ V)(implicit cbf: GBF[K, V]): G[K, V] =
    gtl.asMultiMap[List].withPFKeys(fk).calc(next(grouping))

  private def next[V, K](f: Self[A, F, G] ⇒ V)(implicit cbf: GBF[K, V]): MultiMap[List, K, A] ⇒ G[K, V] =
    inner ⇒ inner.map { case (k, as) ⇒ (k, f(copy(as))) }(breakOut)

  private def zip[K, V](fk: A ⇒ Option[K], fv: A ⇒ Option[V])(a: A): Option[(K, V)] = for { k ← fk(a); v ← fv(a) } yield (k, v)
}

case class UngroupBy[A, B, CC[_]](ungrouped: Map[Int, M.Builder[A, CC[A]]], counts: Map[B, Int])(
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
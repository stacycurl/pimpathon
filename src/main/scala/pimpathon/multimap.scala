package pimpathon

import scala.language.{higherKinds, implicitConversions}

import scala.collection.{breakOut, mutable ⇒ M, GenTraversable}
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{Map ⇒ ▶:}

import pimpathon.any.AnyPimps
import pimpathon.boolean.BooleanPimps
import pimpathon.builder.BuilderPimps
import pimpathon.function.CurriedFunction2Pimps
import pimpathon.map.MapPimps
import pimpathon.stream.StreamPimps
import pimpathon.tuple.Tuple2Pimps


object multiMap {
  type MultiMap[F[_], K, V] = K ▶: F[V]
  type MMCBF[F[_], K, V] = CanBuildFrom[Nothing, (K, V), K ▶: F[V]]

  implicit def build[F[_], K, V](implicit fcbne: CanBuildNonEmpty[V, F[V]]): MMCBF[F, K, V] = MultiMap.build

  implicit class MultiMapPimps[F[_], K, V](val self: K ▶: F[V]) extends AnyVal {
    def select[W](f: F[V] ⇒ W): K ▶: W = self.mapValuesEagerly(f) // just an alias for mapValuesEagerly

    def merge(other: K ▶: F[V])(implicit crf: CanRebuildFrom[F, V]): K ▶: F[V] =
      if (self.isEmpty) other else other.foldLeft(self) {
        case (acc, (key, otherValues)) ⇒ acc.append(key, otherValues)
      }

    def append(key: K, newValues: F[V])(implicit crf: CanRebuildFrom[F, V]): K ▶: F[V] =
      self + ((key, crf.concat(List(self.get(key), Some(newValues)).flatten)))

    def pop(key: K)(implicit crf: CanRebuildFrom[F, V]): K ▶: F[V] = self.updateValue(key, crf.pop)

    def onlyOption(implicit gtl: F[V] <:< GenTraversable[V], crf: CanRebuildFrom[F, V]): Option[K ▶: V] =
      headTailOption.flatMap(_.calcC(head ⇒ tail ⇒ tail.isEmpty.option(head)))

    def sequence(implicit bf: CCBF[K ▶: V, F], gtl: F[V] <:< GenTraversable[V],
      crf: CanRebuildFrom[F, V], crsm: CanRebuildFrom[F, K ▶: V]
    ): F[K ▶: V] = crsm.fromStream(self.unfold(_.headTailOption))

    def headTailOption(implicit gtl: F[V] <:< GenTraversable[V], crf: CanRebuildFrom[F, V])
      : Option[(K ▶: V, K ▶: F[V])] = multiMap.head.filterSelf(_.nonEmpty).map(_ → multiMap.tail)

    def flatMapValues[W](f: V ⇒ F[W])(implicit crfv: CanRebuildFrom[F, V], crfw: CanRebuildFrom[F, W])
      : K ▶: F[W] = self.mapValuesEagerly(crfv.flatMap(_)(f))

    def flatMapValuesU[GW](f: V ⇒ GW)(implicit crfv: CanRebuildFrom[F, V], u: CanRebuildFrom.Unapply[GW])
      : K ▶: u.F[u.V] = self.mapValuesEagerly(crfv.flatMap(_)(f))

    def getOrEmpty(k: K)(implicit fcbf: CCBF[V, F]): F[V] = self.getOrElse(k, fcbf.apply().result())

    def multiMap: MultiMapConflictingPimps[F, K, V] = new MultiMapConflictingPimps[F, K, V](self)
  }


  class MultiMapConflictingPimps[F[_], K, V](private val self: K ▶: F[V]) {
    // These operations cannot be defined on MultiMapPimps because non-implicit methods of the same name exist on Map
    def head(implicit gtl: F[V] <:< GenTraversable[V]): K ▶: V =
      self.flatMap { case (k, fv) ⇒ fv.headOption.map(k → _) }

    def tail(implicit crf: CanRebuildFrom[F, V]): K ▶: F[V] = self.updateValues(crf.pop _)
    def values(implicit crf: CanRebuildFrom[F, V]): F[V] = crf.concat(self.values)

    def reverse(implicit crf: CanRebuildFrom[F, V], cbf: CCBF[K, F]): V ▶: F[K] =
      self.toStream.flatMap(kvs ⇒ crf.toStream(kvs._2).map(_ → kvs._1))(collection.breakOut)

    def mapValues[W](f: V ⇒ W)(implicit crfv: CanRebuildFrom[F, V], crfw: CanRebuildFrom[F, W]): K ▶: F[W] =
      self.mapValuesEagerly(crfv.map(_)(f))

    def mapEntries[C, W](f: K ⇒ F[V] ⇒ (C, F[W]))(
      implicit cbmmf: MMCBF[F, C, F[W]], crf: CanRebuildFrom[F, W], crff: CanRebuildFrom[F, F[W]]
    ): C ▶: F[W] = self.asMultiMap[F].withEntries(f.tupled).mapValuesEagerly(crf.concat)

    def mapEntriesU[C, GW](f: K ⇒ F[V] ⇒ (C, GW))(
      implicit cbmmf: MMCBF[F, C, GW], u: CanRebuildFrom.Unapply[GW], crf: CanRebuildFrom[F, GW]
    ): C ▶: u.F[u.V] = self.asMultiMap[F].withEntries(f.tupled).mapValuesEagerly(u.concat[F](_)(crf))


    def sliding(size: Int)(implicit bf: CCBF[K ▶: V, F], gtl: F[V] <:< GenTraversable[V],
      crf: CanRebuildFrom[F, V], crsm: CanRebuildFrom[F, K ▶: F[V]], fcbf: CanBuildFrom[Nothing, V, F[V]]
    ): F[K ▶: F[V]] = {
      crsm.fromStream(self.unfold(_.headTailOption).sliding(size)
        .map(_.flatMap[(K, V), K ▶: F[V]](_.toStream)(breakOut)).toStream)
    }
  }

  object MultiMap {
    def build[F[_], K, V](implicit fcbne: CanBuildNonEmpty[V, F[V]]): MMCBF[F, K, V] = new MultiMapCanBuildFrom[F, K, V]
    def empty[F[_], K, V]: K ▶: F[V] = Map.empty[K, F[V]]
  }


  trait IgnoreFromCBF[-From, -Elem, +To] extends CanBuildFrom[From, Elem, To] {
    override def apply(from: From): M.Builder[Elem, To] = apply()
  }

  class MultiMapCanBuildFrom[F[_], K, V](implicit fcbne: CanBuildNonEmpty[V, F[V]])
    extends MMCBF[F, K, V] with IgnoreFromCBF[Nothing, (K, V), K ▶: F[V]] {

    def apply(): M.Builder[(K, V), K ▶: F[V]] = new MultiMapBuilder[F, K, V]
  }

  class MultiMapBuilder[F[_], K, V](
    map: M.Map[K, M.Builder[V, F[V]]] = M.Map.empty[K, M.Builder[V, F[V]]]
  )(
    implicit fcbne: CanBuildNonEmpty[V, F[V]]
  )
    extends M.Builder[(K, V), K ▶: F[V]] {

    def +=(elem: (K, V)): this.type = { add(elem._1, elem._2); this }
    def clear(): Unit = map.clear()
    def result(): K ▶: F[V] = map.map(kv ⇒ (kv._1, kv._2.result()))(breakOut)

    private def add(k: K, v: V): Unit = map.put(k, map.get(k).fold(fcbne.builder(v))(_ += v))
  }

  trait CanRebuildFrom[F[_], V] {
    final def concat(ffv: F[F[V]])(implicit crff: CanRebuildFrom[F, F[V]]): F[V] = concat(crff.toStream(ffv))
    final def concat(fvs: Iterable[F[V]]): F[V] = (cbf.apply() +++= fvs.map(toStream)) result()
    final def pop(fv: F[V]): Option[F[V]] = flatMapS(fv)(_.tailOption.filter(_.nonEmpty))
    final def map[W](fv: F[V])(f: V ⇒ W)(implicit gcf: CanRebuildFrom[F, W]): F[W] = gcf.fromStream(toStream(fv).map(f))

    private def flatMapS(fv: F[V])(f: Stream[V] ⇒ Option[Stream[V]]): Option[F[V]] = f(toStream(fv)).map(fromStream)

    final def flatMap[GW](fv: F[V])(f: V ⇒ GW)(implicit gcrf: CanRebuildFrom.Unapply[GW]): gcrf.F[gcrf.V] =
      gcrf.fromStream(toStream(fv).flatMap(v ⇒ gcrf.toStream(f(v))))

    def fromStream(to: TraversableOnce[V]): F[V] = (cbf() ++= to).result()

    def toStream(fv: F[V]): Stream[V]

    protected val cbf: CanBuildFrom[F[V], V, F[V]]
  }

  object CanRebuildFrom {
    trait Unapply[FV] {
      type F[_]
      type V

      def concat[G[_]](g_fv: G[FV])(implicit crf: CanRebuildFrom[G, FV]): F[V] =
        fromStream(for { fv ← crf.toStream(g_fv); v ← toStream(fv) } yield v)

      def fromStream(to: TraversableOnce[V]): F[V]
      def toStream(fv: FV): Stream[V]
    }

    implicit def unapply[F0[_], V0](implicit crf: CanRebuildFrom[F0, V0])
      : Unapply[F0[V0]] { type F[X] = F0[X]; type V = V0 } = new Unapply[F0[V0]] {
      type F[X] = F0[X]
      type V    = V0

      def fromStream(to: TraversableOnce[V]): F[V] = crf.fromStream(to)
      def toStream(fv: F0[V0]): Stream[V] = crf.toStream(fv)
    }

    implicit def crf[F[_], V](
      implicit cbf0: CanBuildFrom[F[V], V, F[V]], fTraversableOnce: F[V] <:< TraversableOnce[V]
    ): CanRebuildFrom[F, V] = new CanRebuildFrom[F, V] {
      def toStream(fv: F[V]): Stream[V] = fTraversableOnce(fv).toStream
      protected val cbf: CanBuildFrom[F[V], V, F[V]] = cbf0
    }
  }
}

trait CanBuildNonEmpty[-Elem, +To] {
  def builder(head: Elem): M.Builder[Elem, To]
}

object CanBuildNonEmpty {
  implicit def canBuildFromToCBNE[Elem, To](
    implicit cbf: CanBuildFrom[Nothing, Elem, To]
  ): CanBuildNonEmpty[Elem, To] = new CanBuildNonEmpty[Elem, To] {
    def builder(head: Elem): M.Builder[Elem, To] = cbf.apply() += head
  }
}
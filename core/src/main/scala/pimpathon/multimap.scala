package pimpathon

import scala.collection.{breakOut, mutable => M, GenTraversable}
import scala.collection.generic.CanBuildFrom

import pimpathon.any._
import pimpathon.builder._
import pimpathon.function._
import pimpathon.map._
import pimpathon.stream._


object multiMap {
  type MultiMap[F[_], K, V] = Map[K, F[V]]
  type MMCBF[F[_], K, V] = CanBuildFrom[Nothing, (K, V), MultiMap[F, K, V]]

  implicit def build[F[_], K, V](implicit fcbne: CanBuildNonEmpty[V, F[V]]): MMCBF[F, K, V] = MultiMap.build

  implicit def multiMapOps[F[_], K, V](multiMap: MultiMap[F, K, V]): MultiMapOps[F, K, V] =
    new MultiMapOps[F, K, V](multiMap)

  class MultiMapOps[F[_], K, V](val value: MultiMap[F, K, V]) {
    def select[W](f: F[V] => W): Map[K, W] = value.mapValuesEagerly(f) // just an alias for mapValuesEagerly

    def merge(other: MultiMap[F, K, V])(implicit crf: CanRebuildFrom[F, V]): MultiMap[F, K, V] =
      if (value.isEmpty) other else other.foldLeft(value) {
        case (acc, (key, otherValues)) => acc.append(key, otherValues)
      }

    def append(key: K, newValues: F[V])(implicit crf: CanRebuildFrom[F, V]): MultiMap[F, K, V] =
      value + ((key, crf.concat(List(value.get(key), Some(newValues)).flatten)))

    def pop(key: K)(implicit crf: CanRebuildFrom[F, V]): MultiMap[F, K, V] = value.updateValue(key, crf.pop)

    def sequence(implicit bf: CCBF[Map[K, V], F], gtl: F[V] <:< GenTraversable[V],
      crf: CanRebuildFrom[F, V], crsm: CanRebuildFrom[F, Map[K, V]]
    ): F[Map[K, V]] = crsm.fromStream(value.unfold(_.headTailOption))

    def headTailOption(implicit gtl: F[V] <:< GenTraversable[V], crf: CanRebuildFrom[F, V])
      : Option[(Map[K, V], MultiMap[F, K, V])] = multiMap.head.filterSelf(_.nonEmpty).map(_ -> multiMap.tail)

    def flatMapValues[W](f: V => F[W])(implicit crfv: CanRebuildFrom[F, V], crfw: CanRebuildFrom[F, W])
      : MultiMap[F, K, W] = value.mapValuesEagerly(crfv.flatMap(_)(f))

    def getOrEmpty(k: K)(implicit fcbf: CCBF[V, F]): F[V] = value.getOrElse(k, fcbf.apply().result())

    def multiMap: MultiMapConflictingOps[F, K, V] = new MultiMapConflictingOps[F, K, V](value)
  }


  class MultiMapConflictingOps[F[_], K, V](value: MultiMap[F, K, V]) {
    // These operations cannot be defined on MultiMapOps because non-implicit methods of the same name exist on Map
    def head(implicit gtl: F[V] <:< GenTraversable[V]): Map[K, V] =
      value.flatMap { case (k, fv) => fv.filterSelf(_.nonEmpty).map(k -> _.head) }

    def tail(implicit crf: CanRebuildFrom[F, V]): MultiMap[F, K, V] = value.updateValues(crf.pop _)
    def values(implicit crf: CanRebuildFrom[F, V]): F[V] = crf.concat(value.values)

    def reverse(implicit crf: CanRebuildFrom[F, V], cbf: CCBF[K, F]): MultiMap[F, V, K] =
      value.toStream.flatMap(kvs => crf.toStream(kvs._2).map(_ -> kvs._1))(collection.breakOut)

    def mapEntries[C, W](f: K => F[V] => (C, F[W]))(
      implicit cbmmf: MMCBF[F, C, F[W]], crf: CanRebuildFrom[F, W], crff: CanRebuildFrom[F, F[W]]
    ): MultiMap[F, C, W] = value.asMultiMap[F].withEntries(f.tupled).mapValuesEagerly(crf.concat)


    def sliding(size: Int)(implicit bf: CCBF[Map[K, V], F], gtl: F[V] <:< GenTraversable[V],
      crf: CanRebuildFrom[F, V], crsm: CanRebuildFrom[F, MultiMap[F, K, V]], fcbf: CanBuildFrom[Nothing, V, F[V]]
    ): F[MultiMap[F, K, V]] = {
      crsm.fromStream(value.unfold(_.headTailOption).sliding(size)
        .map(_.flatMap[(K, V), MultiMap[F, K, V]](_.toStream)(breakOut)).toStream)
    }
  }

  object MultiMap {
    def build[F[_], K, V](implicit fcbne: CanBuildNonEmpty[V, F[V]]): MMCBF[F, K, V] = new MultiMapCanBuildFrom[F, K, V]
    def empty[F[_], K, V]: MultiMap[F, K, V] = Map.empty[K, F[V]]
  }


  trait IgnoreFromCBF[-From, -Elem, +To] extends CanBuildFrom[From, Elem, To] {
    override def apply(from: From): M.Builder[Elem, To] = apply()
  }

  class MultiMapCanBuildFrom[F[_], K, V](implicit fcbne: CanBuildNonEmpty[V, F[V]])
    extends MMCBF[F, K, V] with IgnoreFromCBF[Nothing, (K, V), MultiMap[F, K, V]] {

    def apply(): M.Builder[(K, V), MultiMap[F, K, V]] = new MultiMapBuilder[F, K, V]
  }

  class MultiMapBuilder[F[_], K, V](
    map: M.Map[K, M.Builder[V, F[V]]] = M.Map.empty[K, M.Builder[V, F[V]]]
  )(
    implicit fcbne: CanBuildNonEmpty[V, F[V]]
  )
    extends M.Builder[(K, V), MultiMap[F, K, V]] {

    def +=(elem: (K, V)): this.type = { add(elem._1, elem._2); this }
    def clear(): Unit = map.clear()
    def result(): Map[K, F[V]] = map.map(kv => (kv._1, kv._2.result()))(breakOut)

    private def add(k: K, v: V): Unit = map.put(k, map.get(k).map(_ += v).getOrElse(fcbne.builder(v)))
  }

  trait CanRebuildFrom[F[_], V] {
    def concat(ffv: F[F[V]])(implicit crff: CanRebuildFrom[F, F[V]]): F[V] = concat(crff.toStream(ffv))
    def concat(fvs: Iterable[F[V]]): F[V] = (cbf.apply() +++= fvs.map(toStream)) result()
    def pop(fv: F[V]): Option[F[V]] = flatMapS(fv)(_.tailOption.filter(_.nonEmpty))
    def flatMapS(fv: F[V])(f: Stream[V] => Option[Stream[V]]): Option[F[V]] = f(toStream(fv)).map(fromStream)
    def toStream(fv: F[V]): Stream[V]

    def flatMap[G[_], W](fv: F[V])(f: V => G[W])(implicit gcrf: CanRebuildFrom[G, W]): G[W] =
      gcrf.fromStream(toStream(fv).flatMap(v => gcrf.toStream(f(v))))

    protected val cbf: CanBuildFrom[F[V], V, F[V]]

    def fromStream(to: TraversableOnce[V]): F[V] = (cbf() ++= to).result()
  }

  object CanRebuildFrom {
    trait Unapply[FV] {
      type F[_]
      type V

      def concat[G[_]](g_fv: G[FV])(implicit crf: CanRebuildFrom[G, FV]): F[V] =
        fromStream(for { fv <- crf.toStream(g_fv); v <- toStream(fv) } yield v)

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
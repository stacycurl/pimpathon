package pimpathon

import scala.language.{higherKinds, implicitConversions}

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

  implicit def build[F[_], K, V](implicit fcbf: CanBuildFrom[Nothing, V, F[V]]): MMCBF[F, K, V] = MultiMap.build

  implicit class MultiMapOps[F[_], K, V](val value: MultiMap[F, K, V]) extends AnyVal {
    // just an alias for mapValuesEagerly
    def select[W](f: F[V] => W): Map[K, W] = value.mapValuesEagerly(f)

    def merge(other: MultiMap[F, K, V])(implicit crf: CanRebuildFrom[F, V]): MultiMap[F, K, V] =
      if (value.isEmpty) other else other.foldLeft(value) {
        case (acc, (key, otherValues)) => acc.append(key, otherValues)
      }

    def append(key: K, newValues: F[V])(implicit crf: CanRebuildFrom[F, V]): MultiMap[F, K, V] =
      value + ((key, crf.concat(List(value.get(key), Some(newValues)).flatten)))

    def pop(key: K)(implicit crf: CanRebuildFrom[F, V]): MultiMap[F, K, V] = value.updateValue(key, crf.pop)

    def headTailOption(implicit gtl: F[V] <:< GenTraversable[V], crf: CanRebuildFrom[F, V])
      : Option[(Map[K, V], MultiMap[F, K, V])] = multiMap.head.filterSelf(_.nonEmpty).map(_ -> multiMap.tail)

    def multiMap: MultiMapConflictingOps[F, K, V] = new MultiMapConflictingOps[F, K, V](value)
  }


  class MultiMapConflictingOps[F[_], K, V](value: MultiMap[F, K, V]) {
    // These operations cannot be defined on MultiMapOps because non-implicit methods of the same name exist on Map
    def head(implicit gtl: F[V] <:< GenTraversable[V]): Map[K, V] =
      value.flatMap { case (k, fv) => fv.headOption.map(k -> _) }

    def tail(implicit crf: CanRebuildFrom[F, V]): MultiMap[F, K, V] = value.updateValues(crf.pop _)
    def values(implicit crf: CanRebuildFrom[F, V]): F[V] = crf.concat(value.values)

    def reverse(implicit crf: CanRebuildFrom[F, V], cbf: CCBF[K, F]): MultiMap[F, V, K] =
      value.toStream.flatMap(kvs => crf.toStream(kvs._2).map(_ -> kvs._1))(collection.breakOut)

    def mapEntries[C, W](f: K => F[V] => (C, F[W]))(
      implicit cbmmf: MMCBF[F, C, F[W]], crf: CanRebuildFrom[F, W], crff: CanRebuildFrom[F, F[W]]
    ): MultiMap[F, C, W] = {
      value.asMultiMap[F].withEntries(f.tupled).mapValuesEagerly(crf.concat)
    }
  }

  object MultiMap {
    def build[F[_], K, V](implicit fcbf: CCBF[V, F]): MMCBF[F, K, V] = new MultiMapCanBuildFrom[F, K, V]
    def empty[F[_], K, V]: MultiMap[F, K, V] = Map.empty[K, F[V]]
  }


  trait IgnoreFromCBF[-From, -Elem, +To] extends CanBuildFrom[From, Elem, To] {
    override def apply(from: From): M.Builder[Elem, To] = apply()
  }

  class MultiMapCanBuildFrom[F[_], K, V](implicit fcbf: CCBF[V, F])
    extends MMCBF[F, K, V] with IgnoreFromCBF[Nothing, (K, V), MultiMap[F, K, V]] {

    def apply(): M.Builder[(K, V), MultiMap[F, K, V]] = new MultiMapBuilder[F, K, V]
  }

  class MultiMapBuilder[F[_], K, V](
    map: M.Map[K, M.Builder[V, F[V]]] = M.Map.empty[K, M.Builder[V, F[V]]]
  )(
    implicit fcbf: CCBF[V, F]
  )
    extends M.Builder[(K, V), MultiMap[F, K, V]] {

    def +=(elem: (K, V)): this.type = { add(elem._1, elem._2); this }
    def clear(): Unit = map.clear()
    def result(): Map[K, F[V]] = map.map(kv => (kv._1, kv._2.result()))(breakOut)

    private def add(k: K, v: V): Unit = map.put(k, map.getOrElse(k, fcbf.apply()) += v)
  }

  trait CanRebuildFrom[F[_], V] {
    def concat(ffv: F[F[V]])(implicit crff: CanRebuildFrom[F, F[V]]): F[V] = concat(crff.toStream(ffv))
    def concat(fvs: Iterable[F[V]]): F[V] = (cbf.apply() +++= fvs.map(toStream)) result()
    def pop(fv: F[V]): Option[F[V]] = flatMapS(fv)(_.tailOption.filter(_.nonEmpty))
    def flatMapS(fv: F[V])(f: Stream[V] => Option[Stream[V]]): Option[F[V]] = f(toStream(fv)).map(fromStream)
    def toStream(fv: F[V]): Stream[V]

    protected val cbf: CanBuildFrom[F[V], V, F[V]]

    private def fromStream(to: TraversableOnce[V]): F[V] = (cbf() ++= to).result()
  }

  object CanRebuildFrom {
    implicit def crf[F[_], V](
      implicit cbf0: CanBuildFrom[F[V], V, F[V]], fTraversableOnce: F[V] <:< TraversableOnce[V]
    ): CanRebuildFrom[F, V] = new CanRebuildFrom[F, V] {
      def toStream(fv: F[V]): Stream[V] = fTraversableOnce(fv).toStream
      protected val cbf: CanBuildFrom[F[V], V, F[V]] = cbf0
    }
  }
}

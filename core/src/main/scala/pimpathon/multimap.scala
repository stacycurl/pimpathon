package pimpathon

import scala.collection.{breakOut, mutable => M, GenTraversableLike}
import scala.collection.generic.CanBuildFrom

import pimpathon.map._
import pimpathon.stream._


object multiMap {
  type MultiMap[F[_], K, V] = Map[K, F[V]]

  implicit def build[F[_], K, V](implicit fcbf: CanBuildFrom[Nothing, V, F[V]])
    : CanBuildFrom[Nothing, (K, V), MultiMap[F, K, V]] = MultiMap.build

  implicit class MultiMapOps[F[_], K, V](val value: MultiMap[F, K, V]) extends AnyVal {
    // just an alias for mapValuesEagerly
    def select[W](f: F[V] => W): Map[K, W] = value.mapValuesEagerly(f)

    def merge(other: MultiMap[F, K, V])(implicit crf: CanRebuildFrom[F, V]): MultiMap[F, K, V] =
      if (value.isEmpty) other else other.foldLeft(value) {
        case (acc, (key, otherValues)) => acc.append(key, otherValues)
      }

    def append(key: K, newValues: F[V])(implicit crf: CanRebuildFrom[F, V]): MultiMap[F, K, V] =
      value + ((key, crf.concat(value.get(key), Some(newValues))))

    def pop(key: K)(implicit crf: CanRebuildFrom[F, V]): MultiMap[F, K, V] = value.updateValue(key, crf.pop)

    def multiMap: MultiMapConflictingOps[F, K, V] = new MultiMapConflictingOps[F, K, V](value)
  }


  class MultiMapConflictingOps[F[_], K, V](value: MultiMap[F, K, V]) {
    // These operations cannot be defined on MultiMapOps because non-implicit methods of the same name exist on Map
    def head[Repr](implicit gtl: F[V] <:< GenTraversableLike[V, Repr]): Map[K, V] = value.mapValuesEagerly(_.head)
    def tail(implicit crf: CanRebuildFrom[F, V]): MultiMap[F, K, V] = value.updateValues(crf.pop _)

    def reverse(implicit crf: CanRebuildFrom[F, V], cbf: CanBuildFrom[Nothing, K, F[K]]): MultiMap[F, V, K] =
      value.toStream.flatMap(kvs => crf.toStream(kvs._2).map(_ -> kvs._1))(collection.breakOut)
  }

  object MultiMap {
    def build[F[_], K, V](implicit fcbf: CanBuildFrom[Nothing, V, F[V]])
      : CanBuildFrom[Nothing, (K, V), MultiMap[F, K, V]] = new MultiMapCanBuildFrom[F, K, V]

    def empty[F[_], K, V]: MultiMap[F, K, V] = Map.empty[K, F[V]]
  }


  trait IgnoreFromCBF[-From, -Elem, +To] extends CanBuildFrom[From, Elem, To] {
    override def apply(from: From): M.Builder[Elem, To] = apply()
  }

  class MultiMapCanBuildFrom[F[_], K, V](implicit fcbf: CanBuildFrom[Nothing, V, F[V]])
    extends CanBuildFrom[Nothing, (K, V), MultiMap[F, K, V]]
    with IgnoreFromCBF[Nothing, (K, V), MultiMap[F, K, V]] {

    def apply(): M.Builder[(K, V), MultiMap[F, K, V]] = new MultiMapBuilder[F, K, V]
  }

  class MultiMapBuilder[F[_], K, V](
    map: M.Map[K, M.Builder[V, F[V]]] = M.Map.empty[K, M.Builder[V, F[V]]]
  )(
    implicit fcbf: CanBuildFrom[Nothing, V, F[V]]
  )
    extends M.Builder[(K, V), MultiMap[F, K, V]] {

    def +=(elem: (K, V)): this.type = add(elem._1, elem._2)
    def clear(): Unit = map.clear()
    def result(): Map[K, F[V]] = map.map(kv => (kv._1, kv._2.result()))(breakOut)

    def add(k: K, v: V): this.type = {
      map.put(k, map.getOrElse(k, fcbf.apply()) += v)

      this
    }
  }

  trait CanRebuildFrom[F[_], V] {
    def concat(fvs: Option[F[V]]*): F[V] = fvs.foldLeft(cbf.apply()) {
      case (acc, ofv) => ofv.fold(acc)(fv => acc ++= toStream(fv))
    }.result

    def pop(fv: F[V]): Option[F[V]] = flatMapS(fv)(_.tailOption.filter(_.nonEmpty))
    def flatMapS(fv: F[V])(f: Stream[V] => Option[Stream[V]]): Option[F[V]] = f(toStream(fv)).map(fromStream)
    def toStream(fv: F[V]): Stream[V]

    protected val cbf: CanBuildFrom[F[V], V, F[V]]

    private def fromStream(to: TraversableOnce[V]): F[V] = (cbf() ++= to).result
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

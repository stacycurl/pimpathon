package pimpathon

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable._
import scala.collection.{mutable => M}

import pimpathon.any._
import pimpathon.function._
import pimpathon.multiMap._
import pimpathon.option._
import pimpathon.tuple._


object list {
  implicit def listOps[A](list: List[A]): ListOps[A] = new ListOps[A](list)

  class ListOps[A](list: List[A]) {
    def emptyTo(alternative: => List[A]): List[A] = uncons(alternative, _ => list)

    def uncons[B](empty: => B, nonEmpty: List[A] => B): B = if (list.isEmpty) empty else nonEmpty(list)

    def unconsC[B](empty: => B, nonEmpty: A => List[A] => B): B = list match {
      case Nil => empty
      case head :: tail => nonEmpty(head)(tail)
    }

    def mapNonEmpty[B](f: List[A] => B): Option[B] = if (list.isEmpty) None else Some(f(list))

    def asMap            = as[Map]
    def asMultiMap[F[_]] = as[({ type MM[K, V] = MultiMap[F, K, V] })#MM]

    def as[F[_, _]] = new ListCapturer[A, F](list)

    def zipToMap[B](values: List[B]): Map[A, B] = zip(values).toMap
    def zipWith[B, C](values: List[B])(f: ((A, B)) => C): List[C] = zip(values).map(f).toList


    def attributeCounts[B](f: A => B): Map[B, Int] =
      asMultiMap.withKeys(f).mapValues(_.size)

    def collectAttributeCounts[B](pf: PartialFunction[A, B]): Map[B, Int] =
      optAttributeCounts(pf.lift)

    def optAttributeCounts[B](f: A => Option[B]): Map[B, Int] =
      asMultiMap.withSomeKeys(f).mapValues(_.size)

    def fraction(p: Predicate[A]): Double = countWithSize(p).map(_.to[Double].calc(_ / _)).getOrElse(Double.NaN)

    def countWithSize(p: Predicate[A]): Option[(Int, Int)] = mapNonEmpty(_.foldLeft((0, 0)) {
      case ((passed, size), elem) => (if (p(elem)) passed + 1 else passed, size + 1)
    })


    def distinctBy[B](f: A => B): List[A] = list.map(equalBy(f)).distinct.map(_.a)

    def batchBy[B](f: A => B): List[List[A]] = list.unconsC(empty = Nil, nonEmpty = head => tail => {
      val (_, batch, batches) = tail.foldLeft((f(head), M.ListBuffer(head), M.ListBuffer[List[A]]())) {
        case ((currentKey, batch, batches), a) => f(a).cond(_ == currentKey,
          ifTrue  = key => (key, batch += a,      batches),
          ifFalse = key => (key, M.ListBuffer(a), batches += batch.toList)
        )
      }

      (batches += batch.toList).toList
    })

    def headTail: (A, List[A]) = headTailOption.getOrThrow("headTail of empty list")
    def initLast: (List[A], A) = initLastOption.getOrThrow("initLast of empty list")

    def headTailOption: Option[(A, List[A])] = unconsC(None, head => tail => Some((head, tail)))

    def initLastOption: Option[(List[A], A)] = uncons(None, _ => Some(list.init, list.last))

    def seqMap[B](f: A => Option[B]): Option[List[B]] =
      apoMap[B, Option[List[B]]](Some(_))(a => f(a).toRight(None))

    def tailOption: Option[List[A]] = uncons(None, nonEmpty => Some(nonEmpty.tail))

    def const[B](elem: B): List[B] = list.map(_ => elem)

    def prefixPadTo(len: Int, elem: A): List[A] = List.fill(len - list.length)(elem) ++ list

    def sharedPrefix(other: List[A])(implicit compare: A => A => Boolean = equalC[A]): (List[A], List[A], List[A]) = {
      @tailrec def recurse(lefts: List[A], rights: List[A], acc: List[A]): (List[A], List[A], List[A]) = {
        (lefts, rights) match {
          case (left :: lhs, right :: rhs) if compare(left)(right) => recurse(lhs, rhs, left :: acc)
          case _                                                   => (acc.reverse, lefts, rights)
        }
      }

      recurse(list, other, Nil)
    }

    private def apoMap[B, C](g: List[B] => C)(f: A => Either[C, B]): C = {
      @tailrec def recurse(acc: List[B], rest: List[A]): C = rest match {
        case Nil => g(acc.reverse)
        case head :: tail => f(head) match {
          case Left(c) => c
          case Right(b) => recurse(b :: acc, tail)
        }
      }

      recurse(Nil, list)
    }

    private def equalBy[B](f: A => B)(a: A): EqualBy[A, B] = new EqualBy(f(a))(a)
    private def zip[B](other: List[B]): Iterator[(A, B)] = list.iterator.zip(other.iterator)
  }

  implicit def listTuple2Ops[K, V](list: List[(K, V)]): ListTuple2Ops[K, V] = new ListTuple2Ops[K, V](list)

  class ListTuple2Ops[K, V](list: List[(K, V)]) {
    def toMultiMap[F[_]](implicit fcbf: CanBuildFrom[Nothing, V, F[V]])
      : MultiMap[F, K, V] = list.map(kv => kv)(breakOut)
  }
}

class ListCapturer[A, F[_, _]](list: List[A]) {
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

case class EqualBy[A, B](b: B)(val a: A)

package pimpathon

import scala.{PartialFunction ⇒ ~>}
import scala.annotation.tailrec
import scala.collection.{mutable ⇒ M, GenTraversable, GenTraversableLike}
import scala.collection.immutable._

import pimpathon.genTraversableLike.{GenTraversableLikeOfTuple2Mixin, GenTraversableLikeOfEitherPimpsMixin, GTLGT}

import pimpathon.any._
import pimpathon.boolean._
import pimpathon.function._
import pimpathon.multiMap._
import pimpathon.option._
import pimpathon.ordering._
import pimpathon.tuple._


object list {
  implicit class ListPimps[A](list: List[A]) extends genTraversableLike.GenTraversableLikePimpsMixin[A, List] {
    def tapEmpty[Discarded](empty: ⇒ Discarded): List[A] = tap(empty, _ ⇒ {})
    def tapNonEmpty[Discarded](nonEmpty: List[A] ⇒ Discarded): List[A] = tap({}, nonEmpty)
    def tap[Discarded](empty: ⇒ Discarded, nonEmpty: List[A] ⇒ Discarded): List[A] = { uncons(empty, nonEmpty); list }

    def emptyTo(alternative: ⇒ List[A]): List[A] = uncons(alternative, _ ⇒ list)

    def zipToMap[B](values: List[B]): Map[A, B] = zip(values).toMap
    def zipWith[B, C](values: List[B])(f: ((A, B)) ⇒ C): List[C] = zip(values).map(f).toList

    def fraction(p: Predicate[A]): Double = countWithSize(p).fold(Double.NaN)(_.to[Double].calc(_ / _))

    def countWithSize(p: Predicate[A]): Option[(Int, Int)] = calcIfNonEmpty(_.foldLeft((0, 0)) {
      case ((passed, size), elem) ⇒ (passed + p(elem).asInt, size + 1)
    })

    def sizeGT(value: Int): Boolean = uncons(empty = value < 0, nonEmpty = _.tail.sizeGT(value - 1))

    def duplicates: List[A] = duplicatesBy(identity[A])
    def duplicatesBy[B](f: A ⇒ B): List[A] = (countBy(f) - 1).multiMap.values
    def distinctBy[B](f: A ⇒ B): List[A] = list.map(equalBy(f)).distinct.map(_.a)

    def countBy[B](f: A ⇒ B): MultiMap[List, Int, A] =
      list.asMultiMap[List].withKeys(f).multiMap.mapEntries(_ ⇒ values ⇒ (values.size, values))

    def batchBy[B](f: A ⇒ B): List[List[A]] = list.unconsC(empty = Nil, nonEmpty = head ⇒ tail ⇒ {
      val (_, lastBatch, allBatches) = tail.foldLeft((f(head), M.ListBuffer(head), M.ListBuffer[List[A]]())) {
        case ((currentKey, batch, batches), a) ⇒ f(a).cond(_ == currentKey,
          ifTrue  = key ⇒ (key, batch += a,      batches),
          ifFalse = key ⇒ (key, M.ListBuffer(a), batches += batch.toList)
        )
      }

      (allBatches += lastBatch.toList).toList
    })

    def onlyOrThrow(f: List[A] ⇒ Exception): A = onlyOption.getOrThrow(f(list))
    def onlyEither: Either[List[A], A] = onlyOption.toRight(list)
    def onlyOption: Option[A] = unconsC(None, head ⇒ tail ⇒ tail.headOption.invert(head))

    def headTail: (A, List[A]) = headTailOption.getOrThrow("headTail of empty list")
    def initLast: (List[A], A) = initLastOption.getOrThrow("initLast of empty list")

    def headTailOption: Option[(A, List[A])] = unconsC(None, head ⇒ tail ⇒ Some((head, tail)))
    def initLastOption: Option[(List[A], A)] = uncons(None, nonEmpty ⇒ Some(nonEmpty.init, nonEmpty.last))

    def tailOption: Option[List[A]] = uncons(None, nonEmpty ⇒ Some(nonEmpty.tail))
    def initOption: Option[List[A]] = uncons(None, nonEmpty ⇒ Some(nonEmpty.init))

    def calcIfNonEmpty[B](f: List[A] ⇒ B): Option[B] = list.calcIf(_.nonEmpty)(f)
    def mapIfNonEmpty[B](f: A ⇒ B): Option[List[B]] = list.calcIf(_.nonEmpty)(_.map(f))

    def amass[B](pf: A ~> List[B]): List[B] = list.flatMap(a ⇒ pf.lift(a).getOrElse(Nil))

    def uncons[B](empty: ⇒ B, nonEmpty: List[A] ⇒ B): B = if (list.isEmpty) empty else nonEmpty(list)

    def unconsC[B](empty: ⇒ B, nonEmpty: A ⇒ List[A] ⇒ B): B = list match {
      case Nil          ⇒ empty
      case head :: tail ⇒ nonEmpty(head)(tail)
    }

    def unsnocC[B](empty: ⇒ B, nonEmpty: List[A] ⇒ A ⇒ B): B = initLastOption match {
      case None               ⇒ empty
      case Some((init, last)) ⇒ nonEmpty(init)(last)
    }

    def const[B](elem: B): List[B] = list.map(_ ⇒ elem)

    def prefixPadTo(len: Int, elem: A): List[A] = List.fill(len - list.length)(elem) ++ list

    def sharedPrefix(other: List[A])(implicit compare: A ⇒ A ⇒ Boolean = equalC[A]): (List[A], List[A], List[A]) = {
      @tailrec def recurse(lefts: List[A], rights: List[A], acc: List[A]): (List[A], List[A], List[A]) = {
        (lefts, rights) match {
          case (left :: lhs, right :: rhs) if compare(left)(right) ⇒ recurse(lhs, rhs, left :: acc)
          case _                                                   ⇒ (acc.reverse, lefts, rights)
        }
      }

      recurse(list, other, Nil)
    }

    def zipExact[B](bs: List[B]): (List[(A, B)], Option[Either[List[A], List[B]]]) = zipExactWith(bs)((a, b) ⇒ (a, b))

    def zipExactWith[B, C](other: List[B])(f: (A, B) ⇒ C): (List[C], Option[Either[List[A], List[B]]]) = {
      @tailrec
      def recurse(la: List[A], lb: List[B], cs: List[C]): (List[C], Option[Either[List[A], List[B]]]) = (la, lb) match {
        case (a :: as, b :: bs) ⇒ recurse(as, bs, f(a, b) :: cs)
        case (Nil, Nil)         ⇒ (cs.reverse, None)
        case (as, Nil)          ⇒ (cs.reverse, Some(Left(as)))
        case (Nil, bs)          ⇒ (cs.reverse, Some(Right(bs)))
      }

      recurse(list, other, Nil)
    }

    def sortPromoting(first: A*)(implicit ordering: Ordering[A]): List[A] = list.sorted(ordering.promote(first: _*))
    def sortDemoting(last: A*)(implicit ordering: Ordering[A]): List[A]   = list.sorted(ordering.demote(last: _*))

    private def equalBy[B](f: A ⇒ B)(a: A): EqualBy[A, B] = new EqualBy(f(a))(a)
    private def zip[B](other: List[B]): Iterator[(A, B)] = list.iterator.zip(other.iterator)

    protected def gtl: GenTraversableLike[A, GenTraversable[A]] = list
  }

  implicit class ListOfEithersPimps[L, R](list: List[_ <: Either[L, R]])
    extends GenTraversableLikeOfEitherPimpsMixin[L, R, List] {

    protected def gtl: GTLGT[Either[L, R]] = list
  }

  implicit class ListOfTuple2Pimps[K, V](list: List[(K, V)]) extends GenTraversableLikeOfTuple2Mixin[K, V] {
    protected def gtl: GTLGT[(K, V)] = list
  }

  implicit class MatrixPimps[A](list: List[List[A]]) {
    def cartesianProduct: List[List[A]] = list.foldRight(List(Nil): List[List[A]]) {
      case (item, acc) ⇒ for { a ← item; b ← acc } yield a :: b
    }
  }
}

case class EqualBy[A, B](b: B)(val a: A)
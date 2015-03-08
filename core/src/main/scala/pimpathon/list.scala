package pimpathon

import scala.annotation.tailrec
import scala.collection.{mutable ⇒ M, GenTraversable, GenTraversableLike}
import scala.collection.immutable._

import pimpathon.any._
import pimpathon.boolean._
import pimpathon.function._
import pimpathon.multiMap._
import pimpathon.option._
import pimpathon.tuple._


object list extends genTraversableLike[List] {
  implicit def listPimps[A](list: List[A]): ListPimps[A] = new ListPimps[A](list)
  implicit def matrixPimps[A](matrix: List[List[A]]): MatrixPimps[A] = new MatrixPimps[A](matrix)

  class ListPimps[A](list: List[A]) {
    def tapEmpty(empty: ⇒ Unit): List[A] = tap(empty, _ ⇒ {})
    def tapNonEmpty(nonEmpty: List[A] ⇒ Unit): List[A] = tap({}, nonEmpty)
    def tap(empty: ⇒ Unit, nonEmpty: List[A] ⇒ Unit): List[A] = new AnyPimps(list).tap(_.uncons(empty, nonEmpty))

    def emptyTo(alternative: ⇒ List[A]): List[A] = uncons(alternative, _ ⇒ list)

    def zipToMap[B](values: List[B]): Map[A, B] = zip(values).toMap
    def zipWith[B, C](values: List[B])(f: ((A, B)) ⇒ C): List[C] = zip(values).map(f).toList

    def fraction(p: Predicate[A]): Double = countWithSize(p).map(_.to[Double].calc(_ / _)).getOrElse(Double.NaN)

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

    def onlyOption: Option[A] = unconsC(None, head ⇒ tail ⇒ tail.headOption.invert(head))

    def headTail: (A, List[A]) = headTailOption.getOrThrow("headTail of empty list")
    def initLast: (List[A], A) = initLastOption.getOrThrow("initLast of empty list")

    def headTailOption: Option[(A, List[A])] = unconsC(None, head ⇒ tail ⇒ Some((head, tail)))
    def initLastOption: Option[(List[A], A)] = uncons(None, _ ⇒ Some(list.init, list.last))

    def tailOption: Option[List[A]] = uncons(None, nonEmpty ⇒ Some(nonEmpty.tail))

    def calcIfNonEmpty[B](f: List[A] ⇒ B): Option[B] = list.calcIf(_.nonEmpty)(f)
    def mapIfNonEmpty[B](f: A ⇒ B): Option[List[B]] = list.calcIf(_.nonEmpty)(_.map(f))

    def amass[B](pf: PartialFunction[A, List[B]]): List[B] = list.flatMap(a ⇒ pf.lift(a).getOrElse(Nil))

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

    private def equalBy[B](f: A ⇒ B)(a: A): EqualBy[A, B] = new EqualBy(f(a))(a)
    private def zip[B](other: List[B]): Iterator[(A, B)] = list.iterator.zip(other.iterator)
  }

  class MatrixPimps[A](value: List[List[A]]) {
    def cartesianProduct: List[List[A]] = value.foldRight(List(Nil): List[List[A]]) {
      case (item, acc) ⇒ for { a ← item; b ← acc } yield a :: b
    }
  }

  protected def toGTL[A](l: List[A]): GenTraversableLike[A, GenTraversable[A]] = l
}

case class EqualBy[A, B](b: B)(val a: A)
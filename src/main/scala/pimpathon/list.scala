package pimpathon

import scala.util.Random
import scala.{PartialFunction => ~>}
import scala.annotation.tailrec
import scala.collection.{GenTraversable, GenTraversableLike, mutable => M}
import scala.collection.immutable._
import scala.collection.immutable.{Map => ▶:}
import pimpathon.genTraversableLike.{GTLGT, GenTraversableLikeOfEitherPimpsMixin, GenTraversableLikeOfTuple2Mixin}
import pimpathon.any.AnyPimps
import pimpathon.boolean.BooleanPimps
import pimpathon.function.{Predicate, equalC}
import pimpathon.multiMap.{MultiMapPimps, build}
import pimpathon.option._
import pimpathon.ordering._
import pimpathon.tuple._
import pimpathon.builder.BuilderPimps

import scala.collection.generic.CanBuildFrom


object list {
  implicit class ListPimps[A](self: List[A]) extends genTraversableLike.GenTraversableLikePimpsMixin[A, List] {
    def updateIf(pred: A ⇒ Boolean, updateFn: A ⇒ A): List[A] = self.map {
      case x if pred(x) ⇒ updateFn(x)
      case x            ⇒ x
    }

    def update(pf: A ~> A): List[A] = self.map {
      case x if pf.isDefinedAt(x) => pf(x)
      case x                      => x
    }

    def tapEmpty[Discarded](empty: ⇒ Discarded): List[A] = tap(empty, _ ⇒ {})
    def tapNonEmpty[Discarded](nonEmpty: List[A] ⇒ Discarded): List[A] = tap({}, nonEmpty)
    def tap[Discarded](empty: ⇒ Discarded, nonEmpty: List[A] ⇒ Discarded): List[A] = { uncons(empty, nonEmpty); self }

    def emptyTo(alternative: ⇒ List[A]): List[A] = uncons(alternative, _ ⇒ self)

    def zipToMap[B](values: List[B]): A ▶: B = zip(values).toMap

    case class zipWith[B](values: List[B]) {
      def apply[C, That](f: ((A, B)) ⇒ C)(implicit cbf: CanBuildFrom[List[C], C, That]): That =
        cbf.apply().run(_ ++= zip(values).map(f))
    }

    def fraction(p: Predicate[A]): Double = countWithSize(p).fold(Double.NaN)(_.to[Double].calc(_ / _))

    def countWithSize(p: Predicate[A]): Option[(Int, Int)] = calcIfNonEmpty(_.foldLeft((0, 0)) {
      case ((passed, size), elem) ⇒ (passed + p(elem).asInt, size + 1)
    })

    def sizeGT(value: Int): Boolean = uncons(empty = value < 0, nonEmpty = _.tail.sizeGT(value - 1))

    def duplicates: List[A] = duplicatesBy(identity[A])
    def duplicatesBy[B](f: A ⇒ B): List[A] = (countBy(f) - 1).multiMap.values
    def distinctBy[B](f: A ⇒ B): List[A] = self.map(equalBy(f)).distinct.map(_.a)

    def countBy[B](f: A ⇒ B): Int ▶: List[A] =
      self.asMultiMap[List].withKeys(f).multiMap.mapEntries(_ ⇒ values ⇒ (values.size, values))

    def batchBy[B](f: A ⇒ B): List[List[A]] = self.unconsC(empty = Nil, nonEmpty = head ⇒ tail ⇒ {
      val (_, lastBatch, allBatches) = tail.foldLeft((f(head), M.ListBuffer(head), M.ListBuffer[List[A]]())) {
        case ((currentKey, batch, batches), a) ⇒ f(a).cond(_ == currentKey,
          ifTrue  = key ⇒ (key, batch += a,      batches),
          ifFalse = key ⇒ (key, M.ListBuffer(a), batches += batch.toList)
        )
      }

      (allBatches += lastBatch.toList).toList
    })

    def batchWhile(p: Predicate[List[A]]): List[List[A]] = {
      val (last, res) = self.foldLeft((List.empty[A], List.empty[List[A]])) {
        case ((current, acc), a) ⇒ if (p(a :: current)) (a :: current, acc) else (a :: Nil, current.reverse :: acc)
      }

      (last.reverse :: res).reverse
    }

    def headTail: (A, List[A]) = headTailOption.getOrThrow("headTail of empty list")
    def initLast: (List[A], A) = initLastOption.getOrThrow("initLast of empty list")

    def headTailOption: Option[(A, List[A])] = unconsC(None, head ⇒ tail ⇒ Some((head, tail)))
    def initLastOption: Option[(List[A], A)] = uncons(None, nonEmpty ⇒ Some(nonEmpty.init, nonEmpty.last))

    def tailOption: Option[List[A]] = uncons(None, nonEmpty ⇒ Some(nonEmpty.tail))
    def initOption: Option[List[A]] = uncons(None, nonEmpty ⇒ Some(nonEmpty.init))

    def calcIfNonEmpty[B](f: List[A] ⇒ B): Option[B] = self.calcIf(_.nonEmpty)(f)
    def mapIfNonEmpty[B](f: A ⇒ B): Option[List[B]] = self.calcIf(_.nonEmpty)(_.map(f))

    def amass[B](pf: A ~> List[B]): List[B] = self.flatMap(a ⇒ pf.lift(a).getOrElse(Nil))

    def interleave(rhs: List[A]): List[A] = {
      def recurse(acc: List[A], next: List[A], after: List[A]): List[A] = next match {
        case Nil => acc.reverse ::: after
        case head :: tail => recurse(head :: acc, after, tail)
      }

      recurse(Nil, self, rhs)
    }

    def interleaveWith[B, C](rhs: List[B])(f: Either[A, B] => C): List[C] =
      self.map(a => f(Left(a))).interleave(rhs.map(b => f(Right(b))))

    def uncons[B](empty: ⇒ B, nonEmpty: List[A] ⇒ B): B = if (self.isEmpty) empty else nonEmpty(self)

    def unconsC[B](empty: ⇒ B, nonEmpty: A ⇒ List[A] ⇒ B): B = self match {
      case Nil          ⇒ empty
      case head :: tail ⇒ nonEmpty(head)(tail)
    }

    def unsnocC[B](empty: ⇒ B, nonEmpty: List[A] ⇒ A ⇒ B): B = initLastOption match {
      case None               ⇒ empty
      case Some((init, last)) ⇒ nonEmpty(init)(last)
    }

    def const[B](elem: B): List[B] = self.map(_ ⇒ elem)

    def lpair[B](f: A ⇒ B): List[(B, A)] = self.map(_.lpair(f))
    def rpair[B](f: A ⇒ B): List[(A, B)] = self.map(_.rpair(f))

    def prefixPadTo(len: Int, elem: A): List[A] = List.fill(len - self.length)(elem) ++ self

    def sharedPrefix(other: List[A])(implicit compare: A ⇒ A ⇒ Boolean = equalC[A]): (List[A], List[A], List[A]) = {
      @tailrec def recurse(lefts: List[A], rights: List[A], acc: List[A]): (List[A], List[A], List[A]) = {
        (lefts, rights) match {
          case (left :: lhs, right :: rhs) if compare(left)(right) ⇒ recurse(lhs, rhs, left :: acc)
          case _                                                   ⇒ (acc.reverse, lefts, rights)
        }
      }

      recurse(self, other, Nil)
    }

    def zipExact[B](bs: List[B]): (List[(A, B)], Option[Either[List[A], List[B]]]) = zipExactWith(bs)((a, b) ⇒ (a, b))

    case class zipExactWith[B](other: List[B]) {
      def apply[C](f: (A, B) ⇒ C): (List[C], Option[Either[List[A], List[B]]]) = {
        @tailrec
        def recurse(la: List[A], lb: List[B], cs: List[C]): (List[C], Option[Either[List[A], List[B]]]) = (la, lb) match {
          case (a :: as, b :: bs) ⇒ recurse(as, bs, f(a, b) :: cs)
          case (Nil, Nil)         ⇒ (cs.reverse, None)
          case (as, Nil)          ⇒ (cs.reverse, Some(Left(as)))
          case (Nil, bs)          ⇒ (cs.reverse, Some(Right(bs)))
        }

        recurse(self, other, Nil)
      }
    }

    def sortPromoting(first: A*)(implicit ordering: Ordering[A]): List[A] = self.sorted(ordering.promote(first: _*))
    def sortDemoting(last: A*)(implicit ordering: Ordering[A]): List[A]   = self.sorted(ordering.demote(last: _*))

    def shuffle(): List[A] = Random.shuffle(self)

    private def equalBy[B](f: A ⇒ B)(a: A): EqualBy[A, B] = EqualBy(f(a))(a)
    private def zip[B](other: List[B]): Iterator[(A, B)] = self.iterator.zip(other.iterator)

    protected def gtl: GenTraversableLike[A, GenTraversable[A]] = self
    protected def cc: List[A] = self
  }

  implicit class ListOfEithersPimps[L, R](self: List[_ <: Either[L, R]]) extends GenTraversableLikeOfEitherPimpsMixin[L, R, List] {
    protected def gtl: GTLGT[Either[L, R]] = self
  }

  implicit class ListOfTuple2Pimps[K, V](self: List[(K, V)]) extends GenTraversableLikeOfTuple2Mixin[K, V] {
    def mapFirst[C](f: K => C): List[(C, V)] = mapC(k => v => (f(k), v))
    def mapSecond[W](f: V => W): List[(K, W)] = mapC(k => v => (k, f(v)))
    def mapC[W](f: K ⇒ V ⇒ W): List[W] = self.map(kv ⇒ f(kv._1)(kv._2))

    protected def gtl: GTLGT[(K, V)] = self
  }

  implicit class MatrixPimps[A](val self: List[List[A]]) extends AnyVal {
    def cartesianProduct: List[List[A]] = self.foldRight(List(Nil): List[List[A]]) {
      case (item, acc) ⇒ for { a ← item; b ← acc } yield a :: b
    }
  }
}

case class EqualBy[A, B](b: B)(val a: A)
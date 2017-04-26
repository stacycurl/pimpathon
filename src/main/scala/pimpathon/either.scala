package pimpathon

import scala.language.implicitConversions

import scala.collection.generic.{Growable, Shrinkable}
import scala.util.{Failure, Success, Try}
import scala.{PartialFunction => ~>}

import pimpathon.function.PartialFunctionPimps


object either {
  implicit class EitherPimps[L, R](val self: Either[L, R]) extends AnyVal {
    def leftMap[M](f: L ⇒ M): Either[M, R]  = bimap[M, R](f, identity[R])
    def rightMap[S](f: R ⇒ S): Either[L, S] = bimap[L, S](identity[L], f)

    def valueOr(lr: L ⇒ R): R = rightOr(lr)
    def valueOr(pf: L ~> R): Either[L, R] = rescue(pf)

    def rescue(lr: L ⇒ R): R = rightOr(lr)
    def rescue(pf: L ~> R): Either[L, R] = leftFlatMap(pf.either)

    def leftOr(rl: R ⇒ L): L  = self.fold(identity, rl)
    def rightOr(lr: L ⇒ R): R = self.fold(lr, identity)

    def bimap[M, S](lf: L ⇒ M, rf: R ⇒ S): Either[M, S] = self.fold(l ⇒ Left[M, S](lf(l)), r ⇒ Right[M, S](rf(r)))

    def leftFlatMap(f: L ⇒ Either[L, R]): Either[L, R]  = self.fold(f, Right(_))
    def rightFlatMap(f: R ⇒ Either[L, R]): Either[L, R] = self.fold(Left(_), f)

    def addTo(ls: Growable[L], rs: Growable[R]): Either[L, R] = tap(ls += _, rs += _)
    def removeFrom(ls: Shrinkable[L], rs: Shrinkable[R]): Either[L, R] = tap(ls -= _, rs -= _)

    def tapLeft[Discarded](l: L ⇒ Discarded):  Either[L, R] = tap(l, _ ⇒ {})
    def tapRight[Discarded](r: R ⇒ Discarded): Either[L, R] = tap(_ ⇒ {}, r)
    def tap[Discarded](l: L ⇒ Discarded, r: R ⇒ Discarded): Either[L, R] = { self.fold(l, r); self }

    def getMessage(implicit ev: L <:< Throwable): Option[String] = self.fold(t ⇒ Some(t.getMessage), _ ⇒ None)
    def toTry(implicit ev: L <:< Throwable): Try[R] = self.fold(Failure(_), Success(_))
    def toOption: Option[R] = self.fold(_ => None, Some(_))
  }

  implicit class EitherPimpsNestedL[L, R](val self: Either[Either[L, R], R]) {
    def flatten: Either[L, R] = self.fold(identity, Right(_))
  }

  implicit class EitherFrillsNestedR[L, R](val self: Either[L, Either[L, R]]) {
    def flatten: Either[L, R] = self.fold(Left(_), identity)
  }
}
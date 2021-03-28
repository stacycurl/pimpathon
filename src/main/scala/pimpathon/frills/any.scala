package pimpathon.frills

import scala.language.higherKinds

import argonaut.{CodecJson, Json}
import monocle.Traversal
import pimpathon.Descendant
import pimpathon.argonaut._
import pimpathon.function._
import scalaz.{Failure, NonEmptyList, Success, Validation, ValidationNel}


object any {
  implicit class AnyFrills[A](private val self: A) extends AnyVal {
    def ensure[E](e: ⇒ E)(p: Predicate[A]): Validation[E, A] = if (p(self)) Success(self) else Failure(e)
    def ensureNel[E](e: ⇒ E)(p: Predicate[A]): ValidationNel[E, A] = if (p(self)) Success(self) else Failure(NonEmptyList(e))

    def descendant(paths: String*)(implicit A: CodecJson[A]): Descendant[A, Json, Json] = {
      val aj: Traversal[A, Json] = A.traversalToJson

      Descendant(self,
        paths.map(Descendant.Descender.traversal(aj, _))(collection.breakOut),
        () ⇒ paths.flatMap(Descendant.Descender.ancestors(aj, _))(collection.breakOut)
      )
    }

    def descendant(implicit A: CodecJson[A]): Descendant[A, Json, Json] = {
      val aj: Traversal[A, Json] = A.traversalToJson

      Descendant[A, Json, Json](self, List(aj), () ⇒ List("" -> aj))
    }
  }
}
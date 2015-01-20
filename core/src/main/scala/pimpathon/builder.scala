package pimpathon

import scala.collection.{mutable => M}

import pimpathon.any._


object builder {
  implicit class BuilderOps[A, B](val builder: M.Builder[A, B]) extends AnyVal {
    def +++=(xss: TraversableOnce[TraversableOnce[A]]): M.Builder[A, B] = builder.tap(b => xss.foreach(b ++= _))
  }
}
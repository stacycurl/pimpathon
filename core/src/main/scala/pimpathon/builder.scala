package pimpathon

import scala.collection.mutable

import pimpathon.any._


object builder {
  implicit class BuilderOps[A, B](val builder: mutable.Builder[A, B]) extends AnyVal {
    def +++=(xss: TraversableOnce[TraversableOnce[A]]): mutable.Builder[A, B] = builder.tap(b => xss.foreach(b ++= _))
  }
}
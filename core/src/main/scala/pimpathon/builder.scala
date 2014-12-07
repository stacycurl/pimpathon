package pimpathon

import scala.collection.mutable

import pimpathon.any._


object builder {
  implicit def builderOps[A, B](builder: mutable.Builder[A, B]): BuilderOps[A, B] = new BuilderOps[A, B](builder)

  class BuilderOps[A, B](builder: mutable.Builder[A, B]) {
    def +++=(xss: TraversableOnce[TraversableOnce[A]]): mutable.Builder[A, B] = builder.tap(b => xss.foreach(b ++= _))
  }
}
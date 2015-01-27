package pimpathon

import scala.collection.{mutable => M}

import pimpathon.any._


object builder {
  implicit def builderOps[A, B](builder: M.Builder[A, B]): BuilderOps[A, B] = new BuilderOps[A, B](builder)

  class BuilderOps[A, B](builder: M.Builder[A, B]) {
    def +++=(xss: TraversableOnce[TraversableOnce[A]]): M.Builder[A, B] = builder.tap(b => xss.foreach(b ++= _))
  }
}
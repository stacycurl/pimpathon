package pimpathon

import pimpathon.pimpTry._

import scala.util.{Failure, Success}


class TrySpec extends PSpec {
  "fold" in
    on(Success("foo"), Failure(boom)).calling(_.fold(_.getMessage, s ⇒ s)).produces("foo", boom.getMessage)

  "getMessage" in
    on(Success("foo"), Failure(boom)).calling(_.getMessage).produces(None, Some(boom.getMessage))

  "toEither" in
    on(Success("foo"), Failure(boom)).calling(_.toEither).produces(Right("foo"), Left(boom))
}
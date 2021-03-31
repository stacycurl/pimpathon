package pimpathon.scalaz.std

import pimpathon.PSpec
import pimpathon.scalaz.std.option._
import scalaz.{Failure, Success, NonEmptyList => NEL}

class OptionSpec extends PSpec {
  "toSuccessNel" in values.calling(_.toSuccessNel("fail")).produces(Success(1), Failure(NEL("fail")))
  "toFailureNel" in values.calling(it => it.toFailureNel("succeed")).produces(Failure(NEL(1)), Success("succeed"))

  private lazy val values: on[Option[Int]] = on(Some(1), None)
}
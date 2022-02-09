package pimpathon.frills

import pimpathon.PSpec
import pimpathon.frills.string._
import scalaz.NonEmptyList

class StringTest extends PSpec {
  "splitToNel" - {
    "no seperators" in {
      "no-separators-here-no-siree".splitToNel(",") ≡ NonEmptyList("no-separators-here-no-siree")
    }

    "one seperator" in {
      "first-thing,second-thing".splitToNel(",") ≡ NonEmptyList("first-thing", "second-thing")
    }

    "many seperators" in {
      "first-thing,second-thing,another-thing,yet-another-thing".splitToNel(",") ≡ NonEmptyList(
        "first-thing", "second-thing", "another-thing", "yet-another-thing"
      )
    }
  }
}

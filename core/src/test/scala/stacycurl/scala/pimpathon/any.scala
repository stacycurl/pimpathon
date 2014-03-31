package stacycurl.scala.pimpathon

import org.junit.Test
import scala.collection.mutable.ListBuffer

import org.junit.Assert._
import stacycurl.scala.pimpathon.any._


class AnyTest {
  @Test def tap {
    val tapped = new ListBuffer[Int]

    1.tap(tapped += _)

    assertEquals(List(1), tapped.toList)
  }
}

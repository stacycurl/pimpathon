package pimpathon

import org.junit.Test
import scala.collection.{mutable => M}

import org.junit.Assert._
import pimpathon.any._
import pimpathon.util._


class AnyTest {
  @Test def calc(): Unit = assertEquals(
    List("123", "123"),
    List("12".calc(_ + "3"), "12" |> (_ + "3"))
  )

  @Test def tap(): Unit = {
    val ints = new M.ListBuffer[Int]

    1.tap(ints += _, ints += _)
    assertEquals(List(1, 1), ints.toList)
  }

  @Test def update(): Unit = {
    val ints = new M.ListBuffer[Int]

    1.update(ints += _, ints += _)
    assertEquals(List(1, 1), ints.toList)
  }

  @Test def withSideEffect(): Unit = {
    val ints = new M.ListBuffer[Int]

    1.withSideEffect(ints += _, ints += _)
    assertEquals(List(1, 1), ints.toList)
  }

  @Test def tapIf(): Unit = assertEquals(
    List(1, 3), new M.ListBuffer[Int].tap(ints => {
      List(1, 2, 3).foreach(i => i.tapIf(_ % 2 != 0)(ints += _))
    }).toList
)

  @Test def tapUnless(): Unit = assertEquals(
    List(2), new M.ListBuffer[Int].tap(ints => {
      List(1, 2, 3).foreach(i => i.tapUnless(_ % 2 != 0)(ints += _))
    }).toList
  )

  @Test def tapPF(): Unit = assertEquals(
    List(1, 3), new M.ListBuffer[Int].tap(ints => {
      List(1, 2, 3).foreach(i => i.tapPF { case j if j % 2 != 0 => ints += j })
    })
  )

  @Test def cond(): Unit = assertEquals(
    List("true", "false"),
    List("true", "false").map(_.cond(_ == "true", _ => "true", _ => "false"))
  )

  @Test def partialMatch(): Unit = assertEquals(
    List(Some("Matched"), None),
    List(1, 0).map(i => i partialMatch { case 1 => "Matched" })
  )

  @Test def lpair(): Unit = assertEquals((10, 1), 1.lpair(_ * 10))

  @Test def rpair(): Unit = assertEquals((1, 10), 1.rpair(_ * 10))

  @Test def filterSelf(): Unit = assertEquals(
    List(None, Some(2), None, Some(4)),
    List(1, 2, 3, 4).map(_.filterSelf(_ % 2 == 0))
  )

  @Test def ifSelf(): Unit = assertEquals(
    List(None, Some(2), None, Some(4)),
    List(1, 2, 3, 4).map(_.ifSelf(_ % 2 == 0))
  )

  @Test def filterNotSelf(): Unit = assertEquals(
    List(Some(1), None, Some(3), None),
    List(1, 2, 3, 4).map(_.filterNotSelf(_ % 2 == 0))
  )

  @Test def unlessSelf(): Unit = assertEquals(
    List(Some(1), None, Some(3), None),
    List(1, 2, 3, 4).map(_.unlessSelf(_ % 2 == 0))
  )

  @Test def withFinally(): Unit = assertEquals(
    List("body: input", "finally: input", "done"), new M.ListBuffer[String].tap(strings => {
      strings += "input".withFinally(s => strings += "finally: " + s)(s => {strings += "body: " + s; "done"})
    }).toList
  )

  @Test def attempt(): Unit = {
    assertEquals(Right(2), 1.attempt(_ * 2))
    assertEquals(Left(boom), 1.attempt(_ => throw boom))
  }

  @Test def addTo(): Unit = assertEquals(
    List(1),
    new M.ListBuffer[Int].tap(ints => 1.addTo(ints)).toList
  )
}

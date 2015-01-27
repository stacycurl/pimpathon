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

  @Test def calcIf(): Unit = assertEquals(
    List(Some(5), None, Some(7)),
    List(2, 3, 4).map(_.calcIf(_ % 2 == 0)(_ + 3))
  )

  @Test def calcPF(): Unit = assertEquals(
    List(None, Some("two"), None, Some("four")),
    List(1, 2, 3, 4).map(_.calcPF(util.partial(2 -> "two", 4 -> "four")))
  )

  @Test def transform(): Unit = assertEquals(
    List(1, 4, 3, 8),
    List(1, 2, 3, 4).map(_.transform(util.partial(2 -> 4, 4 -> 8)))
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

  @Test def passes_one(): Unit = {
    assertEquals(List(Some(1), None, None, Some(4)), List(1, 2, 3, 4).map(_.passes.one(_ < 2, _ > 3)))
    assertEquals(List(None, None, None, None),       List(1, 2, 3, 4).map(_.passes.one()))
  }

  @Test def passes_all(): Unit = {
    assertEquals(List(None, Some(2), Some(3), None),       List(1, 2, 3, 4).map(_.passes.all(_ >= 2, _ <= 3)))
    assertEquals(List(Some(1), Some(2), Some(3), Some(4)), List(1, 2, 3, 4).map(_.passes.all()))
  }

  @Test def passes_none(): Unit = {
    assertEquals(List(Some(1), None, None, Some(4)), List(1, 2, 3, 4).map(_.passes.none(_ >= 2, _ <= 3)))
    assertEquals(List(None, None, None, None),       List(1, 2, 3, 4).map(_.passes.none()))
  }

  @Test def passes_some(): Unit = {
    assertEquals(List(None, Some(2), Some(3), None),       List(1, 2, 3, 4).map(_.passes.some(_ < 2, _ > 3)))
    assertEquals(List(Some(1), Some(2), Some(3), Some(4)), List(1, 2, 3, 4).map(_.passes.some()))
  }

  @Test def fails_one(): Unit = {
    assertEquals(List(None, Some(2), Some(3), None),       List(1, 2, 3, 4).map(_.fails.one(_ < 2, _ > 3)))
    assertEquals(List(Some(1), Some(2), Some(3), Some(4)), List(1, 2, 3, 4).map(_.fails.one()))
  }

  @Test def fails_all(): Unit = {
    assertEquals(List(Some(1), None, None, Some(4)), List(1, 2, 3, 4).map(_.fails.all(_ >= 2, _ <= 3)))
    assertEquals(List(None, None, None, None),       List(1, 2, 3, 4).map(_.fails.all()))
  }

  @Test def fails_none(): Unit = {
    assertEquals(List(None, Some(2), Some(3), None),       List(1, 2, 3, 4).map(_.fails.none(_ >= 2, _ <= 3)))
    assertEquals(List(Some(1), Some(2), Some(3), Some(4)), List(1, 2, 3, 4).map(_.fails.none()))
  }

  @Test def fails_some(): Unit = {
    assertEquals(List(Some(1), None, None, Some(4)), List(1, 2, 3, 4).map(_.fails.some(_ < 2, _ > 3)))
    assertEquals(List(None, None, None, None),       List(1, 2, 3, 4).map(_.fails.some()))
  }

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

  @Test def unfold(): Unit = assertEquals(
    List('f', 'o', 'o'), "foo".unfold(s => if (s.nonEmpty) Some(s.head, s.tail) else None).toList
  )
}

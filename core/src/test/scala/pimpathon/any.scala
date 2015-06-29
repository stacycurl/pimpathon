package pimpathon

import org.junit.Test
import scala.util.{Random, Failure, Success}

import org.junit.Assert._
import pimpathon.any._
import pimpathon.boolean._
import pimpathon.builder._
import pimpathon.util._


class AnyTest {
  @Test def calc(): Unit = assertEquals(
    List("123", "123"),
    List("12".calc(_ + "3"), "12" |> (_ + "3"))
  )

  @Test def calcIf(): Unit = on(2, 3, 4).calling(_.calcIf(_ % 2 == 0)(_ + 3)).produces(Some(5), None, Some(7))
  @Test def calcUnless(): Unit = on(2, 3, 4).calling(_.calcUnless(_ % 2 != 0)(_ + 3)).produces(Some(5), None, Some(7))

  @Test def calcPF(): Unit =
    on(1, 2, 3, 4).calling(_.calcPF(util.partial(2 → "two", 4 → "four"))).produces(None, Some("two"), None, Some("four"))

  @Test def transform(): Unit = on(1, 2, 3, 4).calling(_.transform(util.partial(2 → 4, 4 → 8))).produces(1, 4, 3, 8)

  @Test def tap(): Unit            = assertEquals(List(1, 1), ints().run(is ⇒ 1.tap(is += _, is += _)))
  @Test def update(): Unit         = assertEquals(List(1, 1), ints().run(is ⇒ 1.update(is += _, is += _)))
  @Test def withSideEffect(): Unit = assertEquals(List(1, 1), ints().run(is ⇒ 1.withSideEffect(is += _, is += _)))

  @Test def tapIf(): Unit = assertEquals(
    List(2), ints().tap(is ⇒ List(1, 2, 3).foreach(i ⇒ i.tapIf(_ % 2 == 0)(is += _)))
  )

  @Test def tapUnless(): Unit = assertEquals(
    List(1, 3), ints().run(is ⇒ List(1, 2, 3).foreach(i ⇒ i.tapUnless(_ % 2 == 0)(is += _)))
  )

  @Test def tapPF(): Unit = assertEquals(
    List(1, 3), ints().tap(is ⇒ List(1, 2, 3).foreach(i ⇒ i.tapPF { case j if j % 2 != 0 ⇒ is += j }))
  )

  @Test def cond(): Unit = on("true", "false").calling(_.cond(_ == "true", _ ⇒ "T", _ ⇒ "F")).produces("T", "F")

  @Test def partialMatch(): Unit =
    on(1, 0).calling(i ⇒ i partialMatch { case 1 ⇒ "Matched" }).produces(Some("Matched"), None)

  @Test def lpair(): Unit = assertEquals((10, 1), 1.lpair(_ * 10))
  @Test def rpair(): Unit = assertEquals((1, 10), 1.rpair(_ * 10))

  @Test def filterSelf(): Unit = on(1, 2, 3, 4).calling(_.filterSelf(_ % 2 == 0)).produces(None, Some(2), None, Some(4))
  @Test def ifSelf(): Unit     = on(1, 2, 3, 4).calling(_.ifSelf(_ % 2 == 0)).produces(None, Some(2), None, Some(4))

  @Test def filterNotSelf(): Unit =
    on(1, 2, 3, 4).calling(_.filterNotSelf(_ % 2 == 0)).produces(Some(1), None, Some(3), None)

  @Test def unlessSelf(): Unit = on(1, 2, 3, 4).calling(_.unlessSelf(_ % 2 == 0)).produces(Some(1), None, Some(3), None)

  @Test def containedIn(): Unit = on(1, 2, 3, 4).calling(_.containedIn(Set(1, 3))).produces(true, false, true, false)

  @Test def notContainedIn(): Unit = on(1, 2, 3, 4).calling(_.notContainedIn(Set(1, 3))).produces(false, true, false, true)

  @Test def passes_one(): Unit = {
    on(1, 2, 3, 4).calling(_.passes.one(_ < 2, _ > 3)).produces(Some(1), None, None, Some(4))
    on(1, 2, 3, 4).calling(_.passes.one()).produces(None, None, None, None)
  }

  @Test def passes_all(): Unit = {
    on(1, 2, 3, 4).calling(_.passes.all(_ >= 2, _ <= 3)).produces(None, Some(2), Some(3), None)
    on(1, 2, 3, 4).calling(_.passes.all()).produces(Some(1), Some(2), Some(3), Some(4))
  }

  @Test def passes_none(): Unit = {
    on(1, 2, 3, 4).calling(_.passes.none(_ >= 2, _ <= 3)).produces(Some(1), None, None, Some(4))
    on(1, 2, 3, 4).calling(_.passes.none()).produces(None, None, None, None)
  }

  @Test def passes_some(): Unit = {
    on(1, 2, 3, 4).calling(_.passes.some(_ < 2, _ > 3)).produces(None, Some(2), Some(3), None)
    on(1, 2, 3, 4).calling(_.passes.some()).produces(Some(1), Some(2), Some(3), Some(4))
  }

  @Test def fails_one(): Unit = {
    on(1, 2, 3, 4).calling(_.fails.one(_ < 2, _ > 3)).produces(None, Some(2), Some(3), None)
    on(1, 2, 3, 4).calling(_.fails.one()).produces(Some(1), Some(2), Some(3), Some(4))
  }

  @Test def fails_all(): Unit = {
    on(1, 2, 3, 4).calling(_.fails.all(_ >= 2, _ <= 3)).produces(Some(1), None, None, Some(4))
    on(1, 2, 3, 4).calling(_.fails.all()).produces(None, None, None, None)
  }

  @Test def fails_none(): Unit = {
    on(1, 2, 3, 4).calling(_.fails.none(_ >= 2, _ <= 3)).produces(None, Some(2), Some(3), None)
    on(1, 2, 3, 4).calling(_.fails.none()).produces(Some(1), Some(2), Some(3), Some(4))
  }

  @Test def fails_some(): Unit = {
    on(1, 2, 3, 4).calling(_.fails.some(_ < 2, _ > 3)).produces(Some(1), None, None, Some(4))
    on(1, 2, 3, 4).calling(_.fails.some()).produces(None, None, None, None)
  }

  @Test def withFinally(): Unit = assertEquals(
    List("body: input", "finally: input", "done"), strings().run(ss ⇒ {
      ss += "input".withFinally(s ⇒ ss += "finally: " + s)(s ⇒ {ss += "body: " + s; "done"})
    })
  )

  @Test def tryFinally(): Unit = assertEquals(
    List("body: input", "finally: input", "done"), strings().run(ss ⇒ {
      ss += "input".tryFinally(s ⇒ {ss += "body: " + s; "done"})(s ⇒ ss += "finally: " + s)
    })
  )

  @Test def attempt(): Unit = assertEquals(
    List(Success(2), Failure(boom)),
    List(1.attempt(_ * 2), 1.attempt(_ ⇒ throw boom))
  )

  @Test def addTo(): Unit      = assertEquals(List(1), ints().run(is ⇒ 1.addTo(is)))
  @Test def removeFrom(): Unit = assertEquals(Nil,     ints(1).tap(is ⇒ 1.removeFrom(is)).toList)

  @Test def unfold(): Unit = assertEquals(
    List('f', 'o', 'o'), "foo".unfold(s ⇒ s.nonEmpty.option(s.head, s.tail)).toList
  )

  @Test def bounded(): Unit = {
    Stream.fill(10)(Random.nextInt()).foreach(num ⇒ {
      assertEquals((10 max num) min 100, num.bounded(10, 100))
    })

    Stream.fill(10)(Random.nextDouble()).foreach(num ⇒ {
      assertEquals((10.0 max num) min 100.0, num.bounded(10.0, 100.0), 0.01)
    })
  }
}
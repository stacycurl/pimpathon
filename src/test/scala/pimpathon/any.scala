package pimpathon

import org.junit.Test
import scala.util.{Random, Failure, Success}

import org.junit.Assert._
import pimpathon.any._
import pimpathon.boolean._
import pimpathon.builder._
import pimpathon.util._


class AnyTest {
  @Test def calc(): Unit =  List("12".calc(_ + "3"), "12" |> (_ + "3")) === List("123", "123")
  @Test def calcIf(): Unit = on(2, 3, 4).calling(_.calcIf(_ % 2 == 0)(_ + 3)).produces(Some(5), None, Some(7))
  @Test def calcUnless(): Unit = on(2, 3, 4).calling(_.calcUnless(_ % 2 != 0)(_ + 3)).produces(Some(5), None, Some(7))

  @Test def calcPF(): Unit =
    on(1, 2, 3, 4).calling(_.calcPF(util.partial(2 → "two", 4 → "four"))).produces(None, Some("two"), None, Some("four"))

  @Test def transform(): Unit = on(1, 2, 3, 4).calling(_.transform(util.partial(2 → 4, 4 → 8))).produces(1, 4, 3, 8)

  @Test def transformIf(): Unit = on(1, 2, 3, 4).calling(n ⇒ n.transformIf(n % 2 == 0)(_ * 2)).produces(1, 4, 3, 8)
  
  @Test def tap(): Unit = ints().run(is ⇒ 1.tap(is += _, is += _)) === List(1, 1)

  @Test def tapIf(): Unit = ints().run(is ⇒ List(1, 2, 3).foreach(i ⇒ i.tapIf(_ % 2 == 0)(is += _))) === List(2)

  @Test def tapUnless(): Unit =
    ints().run(is ⇒ List(1, 2, 3).foreach(i ⇒ i.tapUnless(_ % 2 == 0)(is += _))) === List(1, 3)

  @Test def tapPF(): Unit =
    ints().run(is ⇒ List(1, 2, 3).foreach(i ⇒ i.tapPF { case j if j % 2 != 0 ⇒ is += j })) === List(1, 3)

  @Test def castTo(): Unit =
    on("foo", 123).calling(_.castTo[String]).produces(Some("foo"), None)

  @Test def cond(): Unit = on("true", "false").calling(_.cond(_ == "true", _ ⇒ "T", _ ⇒ "F")).produces("T", "F")

  @Test def partialMatch(): Unit =
    on(1, 0).calling(i ⇒ i partialMatch { case 1 ⇒ "Matched" }).produces(Some("Matched"), None)

  @Test def lpair(): Unit = 1.lpair(_ * 10) === (10, 1)
  @Test def rpair(): Unit = 1.rpair(_ * 10) === (1, 10)

  @Test def filterSelf(): Unit = on(1, 2, 3, 4).calling(_.filterSelf(_ % 2 == 0)).produces(None, Some(2), None, Some(4))
  @Test def ifSelf(): Unit     = on(1, 2, 3, 4).calling(_.ifSelf(_ % 2 == 0)).produces(None, Some(2), None, Some(4))

  @Test def filterNotSelf(): Unit =
    on(1, 2, 3, 4).calling(_.filterNotSelf(_ % 2 == 0)).produces(Some(1), None, Some(3), None)

  @Test def unlessSelf(): Unit = on(1, 2, 3, 4).calling(_.unlessSelf(_ % 2 == 0)).produces(Some(1), None, Some(3), None)

  @Test def containedIn(): Unit = on(1, 2, 3, 4).calling(_.containedIn(Set(1, 3))).produces(true, false, true, false)
  @Test def notContainedIn(): Unit = on(1, 2, 3, 4).calling(_.notContainedIn(Set(1, 3))).produces(false, true, false, true)

  @Test def isOneOf(): Unit = on(1, 2, 3, 4).calling(_.isOneOf(1, 3)).produces(true, false, true, false)
  @Test def isNotOneOf(): Unit = on(1, 2, 3, 4).calling(_.isNotOneOf(1, 3)).produces(false, true, false, true)

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

  @Test def withFinally(): Unit = strings().run(ss ⇒ {
    ss += "input".withFinally(s ⇒ ss += "finally: " + s)(s ⇒ {ss += "body: " + s; "done"})
  }) === List("body: input", "finally: input", "done")

  @Test def tryFinally(): Unit = strings().run(ss ⇒ {
    ss += "input".tryFinally(s ⇒ {ss += "body: " + s; "done"})(s ⇒ ss += "finally: " + s)
  }) === List("body: input", "finally: input", "done")

  @Test def attempt(): Unit = List(1.attempt(_ * 2), 1.attempt(_ ⇒ throw boom)) === List(Success(2), Failure(boom))

  @Test def addTo(): Unit      = ints().run(is ⇒ 1.addTo(is)) === List(1)
  @Test def removeFrom(): Unit = ints(1).tap(is ⇒ 1.removeFrom(is)).toList === Nil

  @Test def unfold(): Unit = 64.unfold(i ⇒ (i > 1).option((i, i/2))).toList === List(64, 32, 16, 8, 4, 2)

  @Test def bounded(): Unit = {
    Stream.fill(10)(Random.nextInt()).foreach(num ⇒ num.bounded(10, 100) === ((10 max num) min 100))

    Stream.fill(10)(Random.nextDouble()).foreach(num ⇒ {
      assertEquals((10.0 max num) min 100.0, num.bounded(10.0, 100.0), 0.01)
    })
  }
}
package pimpathon

import org.junit.Test
import scala.util.{Failure, Success, Random}

import org.junit.Assert.{assertTrue, assertFalse}
import pimpathon.any._
import pimpathon.function._
import pimpathon.util._


class FunctionTest {
  @Test def attempt(): Unit = {
    ((i: Int) ⇒ i).attempt(3)      === Success(3)
    ((i: Int) ⇒ goBoom).attempt(3) === Failure(boom)
  }

  @Test def guardWith(): Unit =
    on(1, 2, 3, 4).calling((double guardWith isEven).lift).produces(None, Some(4), None, Some(8))

  @Test def unlift(): Unit = f.unlift.calc(pf ⇒ {
    on(0, 1, 2, 3).calling(pf.isDefinedAt).produces(true, false, true, false)
    on(0, 2).calling(pf.apply).produces(0, 2)
    assertThrows[MatchError](pf(1))
    pf.lift === f
  })

  @Test def tuple2(): Unit = f.tuple2.apply(1, 2)          === (None, Some(2))

  private val f: (Int) ⇒ Option[Int] = (i: Int) ⇒ i.filterSelf(_ % 2 == 0)
  private val isEven: Predicate[Int] = _ % 2 == 0
  private val double: (Int ⇒ Int)   = _ * 2
}

class CurriedFunction2Test {
  @Test def tupled(): Unit = ((i: Int) ⇒ (j: Int) ⇒ i + j).tupled((1, 2)) === 3
}

class PredicateTest {
  @Test def cond(): Unit = List(2, 3, 4, 6).map(isEven.cond("even", "odd")) === List("even", "odd", "even", "even")

  @Test def and(): Unit = {
    List(2, 3, 4, 6).filter(isEven and (_ > 2))          === List(4, 6)
    List(2, 3, 4, 6).filter(function.and(isEven, _ > 2)) === List(4, 6)
    assertTrue(function.and[Int]().apply(Random.nextInt()))
  }

  @Test def or(): Unit = {
    List(2, 1, 4, 3, 5).filter(isEven or (_ == 3))          === List(2, 4, 3)
    List(2, 1, 4, 3, 5).filter(function.or(isEven, _ == 3)) === List(2, 4, 3)
    assertFalse(function.or[Int]().apply(Random.nextInt()))
  }

  @Test def not(): Unit = List(2, 1, 4, 3, 5).filter(isEven.not) === List(1, 3, 5)

  @Test def exists(): Unit = List(Nil, List(2), List(3), List(2, 4), List(2, 4, 3)).filter(isEven.exists) ===
    List(List(2), List(2, 4), List(2, 4, 3))

  @Test def forall(): Unit =
    List(Nil, List(2), List(3), List(2, 4), List(2, 4, 3)).filter(isEven.forall) === List(Nil, List(2), List(2, 4))

  @Test def ifSome(): Unit = List(None, Some(3), Some(4), None, Some(6)).filter(isEven.ifSome) ===
    List(Some(4), Some(6))

  @Test def first():  Unit = List((1, 2), (2, 3)).filter(isEven.first[Int])  === List((2, 3))
  @Test def second(): Unit = List((1, 2), (2, 3)).filter(isEven.second[Int]) === List((1, 2))

  @Test def guard(): Unit = on(1, 2, 3, 4).calling((isEven guard double).lift).produces(None, Some(4), None, Some(8))

  @Test def nand(): Unit = {
    List(2, 3, 4, 6).filter(function.nand(isEven, _ > 2)) === List(2, 3)
    assertFalse(function.nand[Int]().apply(Random.nextInt()))
  }

  @Test def nor(): Unit = {
    List(2, 1, 4, 3, 5).filter(function.nor(isEven, _ == 3)) === List(1, 5)
    assertTrue(function.nor[Int]().apply(Random.nextInt()))
  }

  private val isEven: Predicate[Int] = _ % 2 == 0
  private val double: (Int ⇒ Int)   = _ * 2
}

class PartialFunctionTest {
  @Test def either(): Unit  = on(1, 2).calling(util.partial(1 → "2").either).produces(Right("2"), Left(2))
  @Test def toRight(): Unit = on(1, 2).calling(util.partial(1 → "2").toRight).produces(Right("2"), Left(2))
  @Test def toLeft(): Unit  = on(1, 2).calling(util.partial(1 → "2").toLeft).produces(Left("2"), Right(2))
  @Test def unify(): Unit   = on(1, 2).calling(util.partial(2 → 4).unify).produces(1, 4)

  @Test def isUndefinedAt(): Unit =
    on("oof", "foo").calling(util.partial("foo" → "bar").isUndefinedAt).produces(true, false)

  @Test def first(): Unit =
    on(1 → "foo", 2 → "bar").calling(util.partial(1 → 2).first[String].lift).produces(Some(2 → "foo"), None)

  @Test def second(): Unit =
    on("foo" → 1, "bar" → 2).calling(util.partial(1 → 2).second[String].lift).produces(Some("foo" → 2), None)

  @Test def partition(): Unit =
    util.partial(1 → "one", 3 → "three").partition[List](List(1, 2, 3, 4)) === ((List(2, 4), List("one", "three")))

  @Test def starStarStar(): Unit = on((1, 2), (0, 2), (1, 0))
    .calling((util.partial(1 → 2) *** util.partial(2 → 3)).lift).produces(Some((2, 3)), None, None)

  @Test def ampAmpAmp(): Unit = on(1, 2, 3)
    .calling((util.partial(1 → 2, 2 → 3) &&& util.partial(1 → 3, 3 → 4)).lift).produces(Some((2, 3)), None, None)

  @Test def pipePipePipe(): Unit = on(Left(1), Right("two"), Left(3), Right("four"))
    .calling((util.partial(1 → 11) ||| util.partial("two" → 22)).lift).produces(Some(11), Some(22), None, None)

  @Test def map(): Unit = on(1, 2, 3)
    .calling(util.partial(1 → 2, 2 → 3).map(_.toString).lift).produces(Some("2"), Some("3"), None)

  @Test def contramap(): Unit = on(10, 20, 30)
    .calling(util.partial(1 → 2, 2 → 3).contramap[Int](_ / 10).lift).produces(Some(2), Some(3), None)
}
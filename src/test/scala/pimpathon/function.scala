package pimpathon

import scala.util.{Failure, Success, Random}

import org.junit.Assert.{assertTrue, assertFalse}
import pimpathon.any._
import pimpathon.function._


class FunctionSpec extends PSpec {
  "attempt" in {
    ((i: Int) ⇒ i).attempt(3)      ≡ Success(3)
    ((i: Int) ⇒ goBoom).attempt(3) ≡ Failure(boom)
  }

  "guardWith" in
    on(1, 2, 3, 4).calling((double guardWith isEven).lift).produces(None, Some(4), None, Some(8))

  "unlift" in f.unlift.calc(pf ⇒ {
    on(0, 1, 2, 3).calling(pf.isDefinedAt).produces(true, false, true, false)
    on(0, 2).calling(pf.apply).produces(0, 2)
    assertThrows[MatchError](messageDoesntMatter)(pf(1))
    pf.lift ≡ f
  })

  "tuple2" in (f.tuple2.apply(1, 2)          ≡ (None, Some(2)))
  "tuple3" in (f.tuple3.apply(1, 2, 3)       ≡ (None, Some(2), None))
  "tuple4" in (f.tuple4.apply(1, 2, 3, 4)    ≡ (None, Some(2), None, Some(4)))
  "tuple5" in (f.tuple5.apply(1, 2, 3, 4, 5) ≡ (None, Some(2), None, Some(4), None))

  private lazy val f: (Int) ⇒ Option[Int] = (i: Int) ⇒ i.filterSelf(_ % 2 == 0)
  private lazy val isEven: Predicate[Int] = _ % 2 == 0
  private lazy val double: (Int ⇒ Int)   = _ * 2
}

class Function2Spec extends PSpec {
  "tuple2" in (f.tuple2.apply((1, 2),          (10, 20))             ≡ (11, 22))
  "tuple3" in (f.tuple3.apply((1, 2, 3),       (10, 20, 30))         ≡ (11, 22, 33))
  "tuple4" in (f.tuple4.apply((1, 2, 3, 4),    (10, 20, 30, 40))     ≡ (11, 22, 33, 44))
  "tuple5" in (f.tuple5.apply((1, 2, 3, 4, 5), (10, 20, 30, 40, 50)) ≡ (11, 22, 33, 44, 55))

  private lazy val f: (Int, Int) ⇒ Int = _ + _
}

class CurriedFunction2Spec extends PSpec {
  "tupled" in ((i: Int) ⇒ (j: Int) ⇒ i + j).tupled((1, 2)) ≡ 3
}

class PredicateSpec extends PSpec {
  "cond" in List(2, 3, 4, 6).map(isEven.cond("even", "odd")) ≡ List("even", "odd", "even", "even")

  "and" in {
    List(2, 3, 4, 6).filter(isEven and (_ > 2))          ≡ List(4, 6)
    List(2, 3, 4, 6).filter(function.and(isEven, _ > 2)) ≡ List(4, 6)
    assertTrue(function.and[Int]().apply(Random.nextInt()))
  }

  "or" in {
    List(2, 1, 4, 3, 5).filter(isEven or (_ == 3))          ≡ List(2, 4, 3)
    List(2, 1, 4, 3, 5).filter(function.or(isEven, _ == 3)) ≡ List(2, 4, 3)
    assertFalse(function.or[Int]().apply(Random.nextInt()))
  }

  "not" in List(2, 1, 4, 3, 5).filter(isEven.not) ≡ List(1, 3, 5)

  "exists" in List(Nil, List(2), List(3), List(2, 4), List(2, 4, 3)).filter(isEven.exists) ≡
    List(List(2), List(2, 4), List(2, 4, 3))

  "forall" in
    List(Nil, List(2), List(3), List(2, 4), List(2, 4, 3)).filter(isEven.forall) ≡ List(Nil, List(2), List(2, 4))

  "ifSome" in List(None, Some(3), Some(4), None, Some(6)).filter(isEven.ifSome) ≡
    List(Some(4), Some(6))

  "first" in List((1, 2), (2, 3)).filter(isEven.first[Int])  ≡ List((2, 3))
  "second" in List((1, 2), (2, 3)).filter(isEven.second[Int]) ≡ List((1, 2))

  "guard" in on(1, 2, 3, 4).calling((isEven guard double).lift).produces(None, Some(4), None, Some(8))

  "nand" in {
    List(2, 3, 4, 6).filter(function.nand(isEven, _ > 2)) ≡ List(2, 3)
    assertFalse(function.nand[Int]().apply(Random.nextInt()))
  }

  "nor" in {
    List(2, 1, 4, 3, 5).filter(function.nor(isEven, _ == 3)) ≡ List(1, 5)
    assertTrue(function.nor[Int]().apply(Random.nextInt()))
  }

  private lazy val isEven: Predicate[Int] = _ % 2 == 0
  private lazy val double: (Int ⇒ Int)   = _ * 2
}

class PartialFunctionSpec extends PSpec {
  "either"  in on(1, 2).calling(util.partial(1 → "2").either).produces(Right("2"), Left(2))
  "toRight" in on(1, 2).calling(util.partial(1 → "2").toRight).produces(Right("2"), Left(2))
  "toLeft"  in on(1, 2).calling(util.partial(1 → "2").toLeft).produces(Left("2"), Right(2))
  "unify"   in on(1, 2).calling(util.partial(2 → 4).unify).produces(1, 4)

  "isUndefinedAt" in
    on("oof", "foo").calling(util.partial("foo" → "bar").isUndefinedAt).produces(true, false)

  "first" in
    on(1 → "foo", 2 → "bar").calling(util.partial(1 → 2).first[String].lift).produces(Some(2 → "foo"), None)

  "second" in
    on("foo" → 1, "bar" → 2).calling(util.partial(1 → 2).second[String].lift).produces(Some("foo" → 2), None)

  "partition" in
    util.partial(1 → "one", 3 → "three").partition[List](List(1, 2, 3, 4)) ≡ ((List(2, 4), List("one", "three")))

  "***" in on((1, 2), (0, 2), (1, 0))
    .calling((util.partial(1 → 2) *** util.partial(2 → 3)).lift).produces(Some((2, 3)), None, None)

  "&&&" in on(1, 2, 3)
    .calling((util.partial(1 → 2, 2 → 3) &&& util.partial(1 → 3, 3 → 4)).lift).produces(Some((2, 3)), None, None)

  "|||" in on(Left(1), Right("two"), Left(3), Right("four"))
    .calling((util.partial(1 → 11) ||| util.partial("two" → 22)).lift).produces(Some(11), Some(22), None, None)

  "map" in on(1, 2, 3)
    .calling(util.partial(1 → 2, 2 → 3).map(_.toString).lift).produces(Some("2"), Some("3"), None)

  "contramap" in on(10, 20, 30)
    .calling(util.partial(1 → 2, 2 → 3).contramap[Int](_ / 10).lift).produces(Some(2), Some(3), None)

  "partialChain" in {
    def reverse(prefix: Int)(value: String): String = prefix + value.reverse
    def duplicate(prefix: Int)(value: String): String = prefix + value + value

    function.partialChain(reverse, duplicate)(1)("Bolton") ≡ "11notloB1notloB"
  }

  "partialChain2" in {
    def reverse(prefix: Int, suffix: String)(value: String): String = prefix + value.reverse + suffix
    def duplicate(prefix: Int, suffix: String)(value: String): String = prefix + value + value + suffix

    function.partialChain2(reverse, duplicate)(1, "2")("Bolton") ≡ "11notloB21notloB22"
  }
}
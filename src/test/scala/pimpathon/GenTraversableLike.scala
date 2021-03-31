package pimpathon

import scala.language.higherKinds

import scala.collection.{mutable => M}
import scala.{PartialFunction => ~>}
import scala.collection.immutable.SortedMap
import pimpathon.any._
import pimpathon.boolean._
import pimpathon.genTraversableLike._
import pimpathon.multiMap._

import scala.util.Try


class GenTraversableLikeSpec extends PSpec {
  "asMultiMap" - {
    "withKeys" in on(gtl(), gtl(0, 1, 2, 3))
      .calling(_.asMultiMap[List].withKeys(_ % 2), _.asMultiMap[Set].withKeys(_ % 2))
      .produces(Map(), Map(0 → List(0, 2), 1 → List(1, 3)), Map(), Map(0 → Set(0, 2), 1 → Set(1, 3)))
  
    "withValues" in on(gtl(), gtl(0, 1, 2))
      .calling(_.asMultiMap[List].withValues(_ % 2), _.asMultiMap[Set].withValues(_ % 2))
      .produces(Map(), Map(0 → List(0), 1 → List(1), 2 → List(0)), Map(), Map(0 → Set(0), 1 → Set(1), 2 → Set(0)))
  
    "withSomeKeys" in on(gtl(), gtl(0, 1, 2, 3))
      .calling(_.asMultiMap[List].withSomeKeys(optMod2), _.asMultiMap[Set].withSomeKeys(optMod2))
      .produces(Map(), Map(1 → List(1, 3)), Map(), Map(1 → Set(1, 3)))
  
    "withSomeValues" in on(gtl(), gtl(0, 1, 2, 3))
      .calling(_.asMultiMap[List].withSomeValues(optMod2), _.asMultiMap[Set].withSomeValues(optMod2))
      .produces(Map(), Map(1 → List(1), 3 → List(1)), Map(), Map(1 → Set(1), 3 → Set(1)))
  
    "withEntries" - {
      "1 level" in {
        on(gtl(), words).calling(
          _.asMultiMap[List].withEntries(letter(0), s ⇒ s),
          _.asMultiMap[Set].withEntries(letter(0), s ⇒ s)
        ).produces(
          Map(), Map(
            b → List(bar, bare, bzzz),
            f → List(foo, food, faff)
          ),
          Map(), Map(
            b → Set(bar, bare, bzzz),
            f → Set(foo, food, faff)
          )
        )
      }
      
      "2 levels" in {
        on(gtl(), words).calling(
          _.asMultiMap[List].withEntries(letter(0), letter(1), s ⇒ s),
          _.asMultiMap[Set].withEntries(letter(0), letter(1), s ⇒ s)
        ).produces(
          Map(), Map(
            b → Map(a → List(bar, bare), z → List(bzzz)),
            f → Map(o → List(foo, food), a → List(faff))
          ),
          Map(), Map(
            b → Map(a → Set(bar, bare), z → Set(bzzz)),
            f → Map(o → Set(foo, food), a → Set(faff))
          )
        )
      }
    
      "3 levels" in on(gtl(), words).calling(
        _.asMultiMap[List].withEntries(letter(0), letter(1), letter(2), s ⇒ s),
         _.asMultiMap[Set].withEntries(letter(0), letter(1), letter(2), s ⇒ s)
      ).produces(
        Map(), Map(
          b → Map(a → Map(r → List(bar, bare)), z → Map(z → List(bzzz))),
          f → Map(o → Map(o → List(foo, food)), a → Map(f → List(faff)))
        ),
        Map(), Map(
          b → Map(a → Map(r → Set(bar, bare)), z → Map(z → Set(bzzz))),
          f → Map(o → Map(o → Set(foo, food)), a → Map(f → Set(faff)))
        )
      )
    
      "4 levels" in on(gtl(), words).calling(
        _.asMultiMap[List].withEntries(letter(0), letter(1), letter(2), letter(3), s ⇒ s),
         _.asMultiMap[Set].withEntries(letter(0), letter(1), letter(2), letter(3), s ⇒ s)
      ).produces(
        Map(), Map(
          b → Map(a → Map(r → Map(' ' → List(bar), e → List(bare))), z → Map(z → Map(z → List(bzzz)))),
          f → Map(o → Map(o → Map(' ' → List(foo), d → List(food))), a → Map(f → Map(f → List(faff))))
        ),
        Map(), Map(
          b → Map(a → Map(r → Map(' ' → Set(bar), e → Set(bare))), z → Map(z → Map(z → Set(bzzz)))),
          f → Map(o → Map(o → Map(' ' → Set(foo), d → Set(food))), a → Map(f → Map(f → Set(faff))))
        )
      )
    }
    
    "withPFKeys" in on(gtl(), gtl(0, 1, 2, 3)).calling(
      _.asMultiMap[List].withPFKeys { case i if i % 2 == 1 ⇒ i % 2 },
       _.asMultiMap[Set].withPFKeys { case i if i % 2 == 1 ⇒ i % 2 }
    ).produces(Map(), Map(1 → List(1, 3)), Map(), Map(1 → Set(1, 3)))
  
    "withPFValues" in on(gtl(), gtl(0, 1, 2, 3)).calling(
      _.asMultiMap[List].withPFValues { case i if i % 2 == 1 ⇒ i % 2 },
       _.asMultiMap[Set].withPFValues { case i if i % 2 == 1 ⇒ i % 2 }
    ).produces(Map(), Map(1 → List(1), 3 → List(1)), Map(), Map(1 → Set(1), 3 → Set(1)))
  
    "withManyKeys" in on(gtl(), gtl(1, 2)).calling(
      _.asMultiMap[List].withManyKeys(i ⇒ List(i, i + 1)),
       _.asMultiMap[Set].withManyKeys(i ⇒ List(i, i + 1))
    ).produces(
      Map(), Map(1 → List(1), 2 → List(1, 2), 3 → List(2)), Map(), Map(1 → Set(1), 2 → Set(1, 2), 3 → Set(2))
    )
  
    "withUniqueKeys" in on(gtl(), gtl(0, 1), gtl(0, 1, 2)).calling(
      _.asMultiMap[List].withUniqueKeys(_ % 2), _.asMultiMap[Set].withUniqueKeys(_ % 2)
    ).produces(
      Some(Map()), Some(Map(0 → List(0), 1 → List(1))), None, Some(Map()), Some(Map(0 → Set(0), 1 → Set(1))), None
    )
  }
  
  "histogram" - {
    "apply" in gtl(1, 2, 3, 1, 2, 1).histogram.apply() ≡ Map(1 → 3, 2 → 2, 3 → 1)
  
    "opt" in
      gtl("foo", "food", "bar", "oo").histogram.opt(_.length.filterSelf(_ > 2)) ≡ Map(3 → 2, 4 → 1)
  
    "collect" in
      gtl("foo", "food", "bar", "oo").histogram.collect { case word if word.length > 2 ⇒ word.length } ≡
        Map(3 → 2, 4 → 1)

    "by(1)" in on(gtl(), words).calling(_.histogram.by(letter(0))).produces(
      Map(), Map(
        b → 3,
        f → 3
      )
    )

    "by(2)" in on(gtl(), words).calling(_.histogram.by(letter(0), letter(1))).produces(
      Map(), Map(
        b → Map(a → 2, z → 1),
        f → Map(o → 2, a → 1)
      )
    )
    
    "by(3)" in on(gtl(), words).calling(_.histogram.by(letter(0), letter(1), letter(2))).produces(
      Map(), Map(
        b → Map(a → Map(r → 2), z → Map(z → 1)),
        f → Map(o → Map(o → 2), a → Map(f → 1))
      )
    )

    "by(4)" in on(gtl(), words).calling(
      _.histogram.by(letter(0), letter(1), letter(2), letter(3))
    ).produces(
      Map(), Map(
        b → Map(a → Map(r → Map(' ' → 1, e → 1)), z → Map(z → Map(z → 1))),
        f → Map(o → Map(o → Map(' ' → 1, d → 1)), a → Map(f → Map(f → 1)))
      )
    )
  }
  
  "asMap" - {
    "withKeys"       in gtl(1, 2).asMap.withKeys(_ * 2)                  ≡ Map(2 → 1, 4 → 2)
    "withValues"     in gtl(1, 2).asMap.withValues(_ * 2)                ≡ Map(1 → 2, 2 → 4)
    "withConstValue" in gtl(1, 2).asMap.withConstValue(2)                ≡ Map(1 → 2, 2 → 2)
    "withSomeKeys"   in gtl(1, 2, 3, 4).asMap.withSomeKeys(mulPF.lift)   ≡ Map(2 → 1, 6 → 3)
    "withSomeValues" in gtl(1, 2, 3, 4).asMap.withSomeValues(mulPF.lift) ≡ Map(1 → 2, 3 → 6)
    "withEntries"    in gtl(2, 4).asMap.withEntries(entriesFn)           ≡ Map(1 → 4, 2 → 8)
  
    "withSomeEntries" in {
      gtl(1, 2, 3).asMap.withSomeEntries(entriesPF.lift)         ≡ Map(0 → 2, 1 → 6)
      gtl(1, 2, 3).asMap.withSomeEntries(divPF.lift, mulPF.lift) ≡ Map(0 → 2, 1 → 6)
  
      gtl(1, 2, 3).asMap.withSomeEntries(emptyPF[Int, Int].lift, mulPF.lift) ≡ Map()
      gtl(1, 2, 3).asMap.withSomeEntries(divPF.lift, emptyPF[Int, Int].lift) ≡ Map()
    }
  
    "withPFKeys"   in gtl(1, 2).asMap.withPFKeys(mulPF)   ≡ Map(2 → 1)
    "withPFValues" in gtl(1, 2).asMap.withPFValues(mulPF) ≡ Map(1 → 2)
  
    "withPFEntries" in {
      gtl(1, 2, 3).asMap.withPFEntries(entriesPF)                ≡ Map(0 → 2, 1 → 6)
      gtl(1, 2, 3).asMap.withPFEntries(divPF, mulPF)             ≡ Map(0 → 2, 1 → 6)
      gtl(1, 2, 3).asMap.withPFEntries(emptyPF[Int, Int], mulPF) ≡ Map()
      gtl(1, 2, 3).asMap.withPFEntries(divPF, emptyPF[Int, Int]) ≡ Map()
    }

    "withManyKeys" in
      gtl(1, 2).asMap.withManyKeys(i ⇒ List(-i, i)) ≡ Map(-2 → 2, -1 → 1, 1 → 1, 2 → 2)
  
    "withUniqueKeys" in on(gtl(), gtl(1, 2), gtl(1, 2, 3))
      .calling(_.asMap.withUniqueKeys(_ % 2)).produces(Some(Map()), Some(Map(0 → 2, 1 → 1)), None)
  }

  "as[SortedMap]" - {
    "withValues" in gtl(1).as[SortedMap].withValues(_ * 2) ≡ SortedMap(1 → 2)
  }

  "toMultiMap" in on(gtl((1, 10), (1, 11), (2, 20), (2, 21)))
    .calling(_.toMultiMap[List], _.toMultiMap[Set])
    .produces(Map(1 → List(10, 11), 2 → List(20, 21)), Map(1 → Set(10, 11), 2 → Set(20, 21)))

  "all" in on(gtl[Boolean](), gtl(true), gtl(false), gtl(true, true), gtl(false, true))
    .calling(_.all(true)).produces(true, true, false, true, false)

  "none" in on(gtl[Boolean](), gtl(true), gtl(false), gtl(true, true), gtl(false, true))
    .calling(_.none(true)).produces(true, false, true, false, false)

  "seqMap" in {
    gtl[Int]().seqMap[Int, List[Int]](_ ⇒ Some(1)) ≡ Some(Nil)
    gtl(1).seqMap(_ ⇒ None)                        ≡ None
    gtl(1).seqMap[Int, List[Int]](Some(_))          ≡ Some(List(1))

    gtl(1, 2, 3).seqMap {
      case 1 ⇒ Some(1)
      case 2 ⇒ None
      case 3 ⇒ goBoom
    } ≡ None
  }

  "seqFold" in {
    gtl(1, 2, 3).seqFold(0) {
      case (0, j) ⇒ Some(0 + j)
      case (1, _) ⇒ None
      case _      ⇒ goBoom
    } ≡ None

    gtl(1, 2, 3).seqFold(0) { case (i, j) ⇒ Some(i + j) } ≡ Some(6)
  }

  "apoFold" in {
    gtl(1, 2, 3).apoFold(0) {
      case (0, j) ⇒ Right(0 + j)
      case (1, j) ⇒ Left(1 + j)
      case _      ⇒ goBoom
    } ≡ Left(3)

    gtl(1, 2, 3).apoFold(0) { case (i, j) ⇒ Right(i + j) } ≡ Right(6)
  }

  "partitionEithers" in
    gtl(Left(1), Right("abc"), Right("def"), Left(2)).partitionEithers[Set] ≡ (Set(1, 2), Set("abc", "def"))

  "partitionByPF" in
    gtl(1, 2, 3, 4).partitionByPF(util.partial(1 → "one", 3 → "three")) ≡ (gtl(2, 4), gtl("one", "three"))

  "ungroupBy" in gtl('a' → 1, 'a' → 2, 'b' → 1, 'c' → 1, 'b' → 2).ungroupBy(_._1) ≡
    gtl(gtl('a' → 1, 'b' → 1, 'c' → 1), gtl('a' → 2, 'b' → 2))

  "countValues" in {
    val builder: M.Builder[Any, Int] = CountCBF.apply()

    builder.result() ≡ 0
    builder += "foo"
    builder += "bar"
    builder.result() ≡ 2
    builder.clear()
    builder.result() ≡ 0
  }
 
  private def letter(n: Int)(s: String): Char = s.lift(n).getOrElse(' ')
  private def gtl[A](as: A*): GTLGT[A] = as.toList
  private def optMod2(i: Int): Option[Int] = (i % 2 == 1).option(i % 2)
  private lazy val lwords@List(foo, food, faff, bar, bare, bzzz) = List("foo", "food", "faff", "bar", "bare", "bzzz")
  private lazy val words: GTLGT[String] = lwords
  private lazy val List(a,b,d,e,f,m,o,r,z) = "abdefmorz".toList
  private lazy val entriesPF: Int ~> (Int, Int) = { case i if i % 2 == 1 ⇒ (i/2, i*2) }
  private lazy val divPF:    Int ~> Int         = { case i if i % 2 == 1 ⇒ i/2 }
  private lazy val mulPF:  Int ~> Int           = { case i if i % 2 == 1 ⇒ i*2 }
  private lazy val entriesFn: Int ⇒ (Int, Int) = (i: Int) ⇒ (i/2, i*2)
  private def emptyPF[A, B] = util.partial[A, B]()
}
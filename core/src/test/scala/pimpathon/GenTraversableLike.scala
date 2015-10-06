package pimpathon

import scala.language.higherKinds

import scala.{PartialFunction ⇒ ~>}
import scala.collection.immutable.SortedMap

import org.junit.Test

import org.junit.Assert._
import pimpathon.any._
import pimpathon.boolean._
import pimpathon.genTraversableLike._
import pimpathon.multiMap._
import pimpathon.util._


class GenTraversableLikeTests {
  @Test def asMultiMap_withKeys(): Unit = on(gtl(), gtl(0, 1, 2, 3))
    .calling(_.asMultiMap[List].withKeys(_ % 2), _.asMultiMap[Set].withKeys(_ % 2))
    .produces(Map(), Map(0 → List(0, 2), 1 → List(1, 3)), Map(), Map(0 → Set(0, 2), 1 → Set(1, 3)))

  @Test def asMultiMap_withValues(): Unit = on(gtl(), gtl(0, 1, 2))
    .calling(_.asMultiMap[List].withValues(_ % 2), _.asMultiMap[Set].withValues(_ % 2))
    .produces(Map(), Map(0 → List(0), 1 → List(1), 2 → List(0)), Map(), Map(0 → Set(0), 1 → Set(1), 2 → Set(0)))

  @Test def asMultiMap_withSomeKeys(): Unit = on(gtl(), gtl(0, 1, 2, 3))
    .calling(_.asMultiMap[List].withSomeKeys(optMod2), _.asMultiMap[Set].withSomeKeys(optMod2))
    .produces(Map(), Map(1 → List(1, 3)), Map(), Map(1 → Set(1, 3)))

  @Test def asMultiMap_withSomeValues(): Unit = on(gtl(), gtl(0, 1, 2, 3))
    .calling(_.asMultiMap[List].withSomeValues(optMod2), _.asMultiMap[Set].withSomeValues(optMod2))
    .produces(Map(), Map(1 → List(1), 3 → List(1)), Map(), Map(1 → Set(1), 3 → Set(1)))

  @Test def asMultiMap_withEntries(): Unit = on(gtl(), gtl((1, 10) → 100, (1, 11) → 110, (2, 20) → 200)).calling(
    _.asMultiMap[List].withEntries { case ((i, j), k) ⇒ (i, (j, k)) },
     _.asMultiMap[Set].withEntries { case ((i, j), k) ⇒ (i, (j, k)) }
  ).produces(
    Map(), Map(1 → List((10, 100), (11, 110)), 2 → List((20, 200))),
    Map(), Map(1 → Set((10, 100), (11, 110)),  2 → Set((20, 200)))
  )

  @Test def asMultiMap_withEntries_2(): Unit = on(gtl(), gtl((1, 10) → 100, (1, 11) → 110, (2, 20) → 200)).calling(
    _.asMultiMap[List].withEntries({ case ((i, j), k) ⇒ i }, { case ((i, j), k) ⇒ (j, k) }),
     _.asMultiMap[Set].withEntries({ case ((i, j), k) ⇒ i }, { case ((i, j), k) ⇒ (j, k) })
  ).produces(
    Map(), Map(1 → List((10, 100), (11, 110)), 2 → List((20, 200))),
    Map(), Map(1 → Set((10, 100), (11, 110)),  2 → Set((20, 200)))
  )

  @Test def asMultiMap_withEntries_nested(): Unit = on(gtl(), words).calling(
    _.asMultiMap[List].withEntries(letter(0), letter(1), s ⇒ s),
     _.asMultiMap[Set].withEntries(letter(0), letter(1), s ⇒ s)
  ).produces(
    Map(), Map(
      b → Map(a → List(bar, bare), o → List(boom)),
      f → Map(o → List(foo, food), a → List(faff))
    ),
    Map(), Map(
      b → Map(a → Set(bar, bare), o → Set(boom)),
      f → Map(o → Set(foo, food), a → Set(faff))
    )
  )

  @Test def asMultiMap_withEntries_nested_2(): Unit = on(gtl(), words).calling(
    _.asMultiMap[List].withEntries(letter(0), letter(1), letter(2), s ⇒ s),
     _.asMultiMap[Set].withEntries(letter(0), letter(1), letter(2), s ⇒ s)
  ).produces(
    Map(), Map(
      b → Map(a → Map(r → List(bar, bare)), o → Map(o → List(boom))),
      f → Map(o → Map(o → List(foo, food)), a → Map(f → List(faff)))
    ),
    Map(), Map(
      b → Map(a → Map(r → Set(bar, bare)), o → Map(o → Set(boom))),
      f → Map(o → Map(o → Set(foo, food)), a → Map(f → Set(faff)))
    )
  )

  @Test def asMultiMap_withEntries_nested_3(): Unit = on(gtl(), words).calling(
    _.asMultiMap[List].withEntries(letter(0), letter(1), letter(2), letter(3), s ⇒ s),
     _.asMultiMap[Set].withEntries(letter(0), letter(1), letter(2), letter(3), s ⇒ s)
  ).produces(
    Map(), Map(
      b → Map(a → Map(r → Map(' ' → List(bar), e → List(bare))), o → Map(o → Map(m → List(boom)))),
      f → Map(o → Map(o → Map(' ' → List(foo), d → List(food))), a → Map(f → Map(f → List(faff))))
    ),
    Map(), Map(
      b → Map(a → Map(r → Map(' ' → Set(bar), e → Set(bare))), o → Map(o → Map(m → Set(boom)))),
      f → Map(o → Map(o → Map(' ' → Set(foo), d → Set(food))), a → Map(f → Map(f → Set(faff))))
    )
  )

  @Test def asMultiMap_withPFKeys(): Unit = on(gtl(), gtl(0, 1, 2, 3)).calling(
    _.asMultiMap[List].withPFKeys { case i if i % 2 == 1 ⇒ i % 2 },
     _.asMultiMap[Set].withPFKeys { case i if i % 2 == 1 ⇒ i % 2 }
  ).produces(Map(), Map(1 → List(1, 3)), Map(), Map(1 → Set(1, 3)))

  @Test def asMultiMap_withPFValues(): Unit = on(gtl(), gtl(0, 1, 2, 3)).calling(
    _.asMultiMap[List].withPFValues { case i if i % 2 == 1 ⇒ i % 2 },
     _.asMultiMap[Set].withPFValues { case i if i % 2 == 1 ⇒ i % 2 }
  ).produces(Map(), Map(1 → List(1), 3 → List(1)), Map(), Map(1 → Set(1), 3 → Set(1)))

  @Test def asMultiMap_withManyKeys(): Unit = on(gtl(), gtl(1, 2)).calling(
    _.asMultiMap[List].withManyKeys(i ⇒ List(i, i + 1)),
     _.asMultiMap[Set].withManyKeys(i ⇒ List(i, i + 1))
  ).produces(
    Map(), Map(1 → List(1), 2 → List(1, 2), 3 → List(2)), Map(), Map(1 → Set(1), 2 → Set(1, 2), 3 → Set(2))
  )

  @Test def asMultiMap_withUniqueKeys(): Unit = on(gtl(), gtl(0, 1), gtl(0, 1, 2)).calling(
    _.asMultiMap[List].withUniqueKeys(_ % 2), _.asMultiMap[Set].withUniqueKeys(_ % 2)
  ).produces(
    Some(Map()), Some(Map(0 → List(0), 1 → List(1))), None, Some(Map()), Some(Map(0 → Set(0), 1 → Set(1))), None
  )

  @Test def asMap_withKeys(): Unit       = assertEquals(Map(2→1, 4→2), gtl(1,2).asMap.withKeys(_ * 2))
  @Test def asMap_withValues(): Unit     = assertEquals(Map(1→2, 2→4), gtl(1,2).asMap.withValues(_ * 2))
  @Test def asMap_withConstValue(): Unit = assertEquals(Map(1→2, 2→2), gtl(1,2).asMap.withConstValue(2))
  @Test def asMap_withSomeKeys(): Unit   = assertEquals(Map(2→1, 6→3), gtl(1,2,3,4).asMap.withSomeKeys(mulPF.lift))
  @Test def asMap_withSomeValues(): Unit = assertEquals(Map(1→2, 3→6), gtl(1,2,3,4).asMap.withSomeValues(mulPF.lift))
  @Test def asMap_withEntries(): Unit    = assertEquals(Map(1→4, 2→8), gtl(2,4).asMap.withEntries(entriesFn))

  @Test def asMap_withSomeEntries(): Unit = {
    assertEquals(Map(0 → 2, 1 → 6), gtl(1, 2, 3).asMap.withSomeEntries(entriesPF.lift))
    assertEquals(Map(0 → 2, 1 → 6), gtl(1, 2, 3).asMap.withSomeEntries(divPF.lift, mulPF.lift))

    assertEquals(Map(), gtl(1, 2, 3).asMap.withSomeEntries(emptyPF[Int, Int].lift, mulPF.lift))
    assertEquals(Map(), gtl(1, 2, 3).asMap.withSomeEntries(divPF.lift, emptyPF[Int, Int].lift))
  }

  @Test def asMap_withPFKeys(): Unit   = assertEquals(Map(2 → 1), gtl(1, 2).asMap.withPFKeys(mulPF))
  @Test def asMap_withPFValues(): Unit = assertEquals(Map(1 → 2), gtl(1, 2).asMap.withPFValues(mulPF))

  @Test def asMap_withPFEntries(): Unit = {
    assertEquals(Map(0 → 2, 1 → 6), gtl(1, 2, 3).asMap.withPFEntries(entriesPF))
    assertEquals(Map(0 → 2, 1 → 6), gtl(1, 2, 3).asMap.withPFEntries(divPF, mulPF))
    assertEquals(Map(), gtl(1, 2, 3).asMap.withPFEntries(emptyPF[Int, Int], mulPF))
    assertEquals(Map(), gtl(1, 2, 3).asMap.withPFEntries(divPF, emptyPF[Int, Int]))
  }

  @Test def as_SortedMap_withValues(): Unit = assertEquals(Map(1 → 2), gtl(1).as[SortedMap].withValues(_ * 2))

  @Test def asMap_withManyKeys(): Unit =
    assertEquals(Map(-2 → 2, -1 → 1, 1 → 1, 2 → 2), gtl(1, 2).asMap.withManyKeys(i ⇒ List(-i, i)))

  @Test def asMap_withUniqueKeys(): Unit = on(gtl(), gtl(1, 2), gtl(1, 2, 3))
    .calling(_.asMap.withUniqueKeys(_ % 2)).produces(Some(Map()), Some(Map(0 → 2, 1 → 1)), None)

  @Test def attributeCounts(): Unit = assertEquals(Map(3 → 2, 4 → 1), gtl("foo", "food", "bar").attributeCounts(_.size))

  @Test def optAttributeCounts(): Unit =
    assertEquals(Map(3 → 2, 4 → 1), gtl("foo", "food", "bar", "oo").optAttributeCounts(_.size.filterSelf(_ > 2)))

  @Test def collectAttributeCounts(): Unit =assertEquals(
    Map(3 → 2, 4 → 1), gtl("foo", "food", "bar", "oo").collectAttributeCounts { case word if word.size > 2 ⇒ word.size }
  )

  @Test def toMultiMap(): Unit = on(gtl((1, 10), (1, 11), (2, 20), (2, 21)))
    .calling(_.toMultiMap[List], _.toMultiMap[Set])
    .produces(Map(1 → List(10, 11), 2 → List(20, 21)), Map(1 → Set(10, 11), 2 → Set(20, 21)))

  @Test def all(): Unit = on(gtl[Boolean](), gtl(true), gtl(false), gtl(true, true), gtl(false, true))
    .calling(_.all(true)).produces(true, true, false, true, false)

  @Test def none(): Unit = on(gtl[Boolean](), gtl(true), gtl(false), gtl(true, true), gtl(false, true))
    .calling(_.none(true)).produces(true, false, true, false, false)

  @Test def seqMap(): Unit = {
    assertEquals(Some(Nil),      gtl[Int]().seqMap(_ ⇒ Some(1)))
    assertEquals(None,           gtl(1).seqMap(_ ⇒ None))
    assertEquals(Some(List(1)),  gtl(1).seqMap(Some(_)))

    assertEquals(None, gtl(1, 2, 3).seqMap {
      case 1 ⇒ Some(1)
      case 2 ⇒ None
      case 3 ⇒ goBoom
    })
  }

  @Test def seqFold(): Unit = {
    assertEquals(None, gtl(1, 2, 3).seqFold(0) {
      case (0, j) ⇒ Some(0 + j)
      case (1, _) ⇒ None
      case _      ⇒ goBoom
    })

    assertEquals(Some(6), gtl(1, 2, 3).seqFold(0) { case (i, j) ⇒ Some(i + j) })
  }

  @Test def apoFold(): Unit = {
    assertEquals(Left(3), gtl(1, 2, 3).apoFold(0) {
      case (0, j) ⇒ Right(0 + j)
      case (1, j) ⇒ Left(1 + j)
      case _      ⇒ goBoom
    })

    assertEquals(Right(6), gtl(1, 2, 3).apoFold(0) { case (i, j) ⇒ Right(i + j) })
  }

  @Test def partitionEithers(): Unit = assertEquals(
    (Set(1, 2), Set("abc", "def")),
    gtl(Left(1), Right("abc"), Right("def"), Left(2)).partitionEithers[Set]
  )

  @Test def partitionByPF(): Unit = assertEquals(
    (gtl(2, 4), gtl("one", "three")),
    gtl(1, 2, 3, 4).partitionByPF(util.partial(1 → "one", 3 → "three"))
  )

  @Test def ungroupBy(): Unit = assertEquals(
    gtl(gtl('a' → 1, 'b' → 1, 'c' → 1), gtl('a' → 2, 'b' → 2)),
    gtl('a' → 1, 'a' → 2, 'b' → 1, 'c' → 1, 'b' → 2).ungroupBy(_._1)
  )

  private def letter(n: Int)(s: String): Char = s.lift(n).getOrElse(' ')
  private def gtl[A](as: A*): GTLGT[A] = as.toList
  private def optMod2(i: Int): Option[Int] = (i % 2 == 1).option(i % 2)
  private val lwords@List(foo, food, faff, bar, bare, boom) = List("foo", "food", "faff", "bar", "bare", "boom")
  private val words: GTLGT[String] = lwords
  private val List(a,b,d,e,f,m,o,r) = "abdefmor".toList
  private val entriesPF: Int ~> (Int, Int) = { case i if i % 2 == 1 ⇒ (i/2, i*2) }
  private val divPF:    Int ~> Int         = { case i if i % 2 == 1 ⇒ i/2 }
  private val mulPF:  Int ~> Int           = { case i if i % 2 == 1 ⇒ i*2 }
  private val entriesFn: Int ⇒ (Int, Int) = (i: Int) ⇒ (i/2, i*2)
  private def emptyPF[A, B] = util.partial[A, B]()
}
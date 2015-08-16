package pimpathon

import org.junit.Test
import pimpathon.function.Predicate
import scala.collection.{mutable ⇒ M}

import org.junit.Assert._
import pimpathon.any._
import pimpathon.stream._


class StreamTest {
  @Test def cond(): Unit = {
    assertEquals(Stream.empty[Int], stream.cond(cond = false, util.goBoom))
    assertEquals(Stream(1, 2, 3), stream.cond(cond = true, Stream(1, 2, 3)))
  }

  @Test def continuallyWhile(): Unit = {
    assertEquals(Stream.empty[Int], stream.continuallyWhile(1)(_ ⇒ false))
    assertEquals(List.fill(1000)(1), stream.continuallyWhile(1)(_ ⇒ true).take(1000).toList)

    val ints = M.Stack[Int](1, 2, 3)
    assertEquals(List(1, 2), stream.continuallyWhile(ints.pop())(_ < 3).toList)
  }

  @Test def uncons(): Unit = {
    assertEquals("empty", Stream.empty[Int].uncons("empty", s ⇒ s"size: ${s.size}"))
    assertEquals("size: 3", Stream(1, 2, 3).uncons("empty", s ⇒ s"size: ${s.size}"))
  }

  @Test def unconsC(): Unit = {
    assertEquals("empty", Stream.empty[Int].unconsC("empty", h ⇒ t ⇒ s"size: ${1 + t.size}"))
    assertEquals("size: 3", Stream(1, 2, 3).unconsC("empty", h ⇒ t ⇒ s"size: ${1 + t.size}"))
  }

  @Test def tailOption(): Unit = {
    assertEquals(None,                     Stream.empty[Int].tailOption)
    assertEquals(Some(Stream.empty[Int]),  Stream(0).tailOption)
    assertEquals(Some(Stream(1)),          Stream(0, 1).tailOption)
  }

  @Test def lazyScanLeft(): Unit =
    assertEquals(List(0, 1, 3, 6), blockingInts(start = 1, end = 4).lazyScanLeft(0)(_ + _).take(4).toList)

  @Test def reverseInits(): Unit = assertEquals(
    List(Nil, List(1), List(1, 2), List(1, 2, 3)),
    blockingInts(start = 1, end = 4).reverseInits.take(4).toList.map(_.toList)
  )

  private def blockingInts(start: Int, end: Int): Stream[Int] = blockWhen(Stream.iterate(start)(_ + 1))(_ == end)
  private def blockWhen[A](in: Stream[A])(p: Predicate[A]): Stream[A] = in.map(_.tapIf(p)(_ ⇒ block()))
  private def block() = this.synchronized(this.wait(0))
}
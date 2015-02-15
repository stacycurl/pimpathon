package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.filterMonadic._


class FilterMonadicTest {
  @Test def toMultiMap(): Unit = {
    assertEquals(Map(), Set.empty[(Int, Int)].toMultiMap[List])
    assertEquals(Map(), List.empty[(Int, Int)].toMultiMap[List])


    assertEquals(Map(1 → List(10, 11), 2 → List(20, 21)),
      Set((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[List])

    assertEquals(Map(1 → List(10, 11), 2 → List(20, 21)),
      List((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[List])


    assertEquals(Map(1 → Set(10, 11), 2 → Set(20, 21)),
      Set((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[Set])

    assertEquals(Map(1 → Set(10, 11), 2 → Set(20, 21)),
      List((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[Set])
  }
}
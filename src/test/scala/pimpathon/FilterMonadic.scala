package pimpathon

import org.junit.Test

import pimpathon.filterMonadic._
import pimpathon.util._


class FilterMonadicTest {
  @Test def toMultiMap(): Unit = {
    Set.empty[(Int, Int)].toMultiMap[List]  === Map()
    List.empty[(Int, Int)].toMultiMap[List] === Map()

    Set((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[List]  === Map(1 → List(10, 11), 2 → List(20, 21))
    List((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[List] === Map(1 → List(10, 11), 2 → List(20, 21))

    Set((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[Set]  === Map(1 → Set(10, 11), 2 → Set(20, 21))
    List((1, 10), (1, 11), (2, 20), (2, 21)).toMultiMap[Set] === Map(1 → Set(10, 11), 2 → Set(20, 21))
  }
}
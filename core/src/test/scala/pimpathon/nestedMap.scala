package pimpathon

import org.junit.Test

import org.junit.Assert._
import pimpathon.builder._
import pimpathon.nestedMap._


class NestedMapTest {
  @Test def nestedMapCBF(): Unit = {
    val cbf = NestedMap.build[String, Int, String]
    val builder = cbf.apply()

    builder += (("one", 1, "foo")) += (("two", 2, "bar"))
    assertEquals(Map("one" → Map(1 → "foo"), "two" → Map(2 → "bar")), builder.reset())
    assertEquals(NestedMap.empty[String, Int, String], builder.reset())
  }

  @Test def nestedMap_mapValuesEagerly(): Unit = assertEquals(
    Map(1 → Map(2 → 6, 3 → 8), 2 → Map(3 → 8, 4 → 10)),
    Map(1 → Map(2 → 3, 3 → 4), 2 → Map(3 → 4, 4 → 5)).nestedMap.mapValuesEagerly(_ * 2)
  )

  @Test def nestedMap_mapKeysEagerly(): Unit = assertEquals(
    Map(1 → Map(4 → 3, 6 → 4), 2 → Map(6 → 4, 8 → 5)),
    Map(1 → Map(2 → 3, 3 → 4), 2 → Map(3 → 4, 4 → 5)).nestedMap.mapKeysEagerly(_ * 2)
  )

  @Test def flipNesting(): Unit = assertEquals(
    Map(2 → Map(10 → 3), 3 → Map(10 → 4, 20 → 4), 4 → Map(20 → 5)),
    Map(10 → Map(2 → 3, 3 → 4), 20 → Map(3 → 4, 4 → 5)).flipNesting
  )

  @Test def append(): Unit = {
    assertEquals(Map(1 → Map(2 → 4)),                 Map(1 → Map(2 → 3)).append(1, 2, 4))
    assertEquals(Map(1 → Map(2 → 3, 3 → 4)),          Map(1 → Map(2 → 3)).append(1, 3, 4))
    assertEquals(Map(1 → Map(2 → 3), 2 → Map(3 → 4)), Map(1 → Map(2 → 3)).append(2, 3, 4))

    assertEquals(Map(1 → Map(2 → 4)),                 Map(1 → Map(2 → 3)) + ((1, 2, 4)))
    assertEquals(Map(1 → Map(2 → 3, 3 → 4)),          Map(1 → Map(2 → 3)) + ((1, 3, 4)))
    assertEquals(Map(1 → Map(2 → 3), 2 → Map(3 → 4)), Map(1 → Map(2 → 3)) + ((2, 3, 4)))
  }

  @Test def getOrEmpty(): Unit = {
    assertEquals(Map(2 → 3), Map(1 → Map(2 → 3)).getOrEmpty(1))
    assertEquals(Map(),      Map(1 → Map(2 → 3)).getOrEmpty(2))
  }
}
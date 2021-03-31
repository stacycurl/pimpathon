package pimpathon

import pimpathon.builder._
import pimpathon.nestedMap._


class NestedMapSpec extends PSpec {
  "nestedMapCBF" in {
    val cbf = NestedMap.build[String, Int, String]
    val builder = cbf.apply()

    builder += (("one", 1, "foo")) += (("two", 2, "bar"))
    builder.reset() ≡ Map("one" → Map(1 → "foo"), "two" → Map(2 → "bar"))
    builder.reset() ≡ NestedMap.empty[String, Int, String]
  }

  "nestedMap_mapValuesEagerly" in
    Map(1 → Map(2 → 3, 3 → 4), 2 → Map(3 → 4, 4 → 5)).nestedMap.mapValuesEagerly(_ * 2) ≡
      Map(1 → Map(2 → 6, 3 → 8), 2 → Map(3 → 8, 4 → 10))

  "nestedMap_mapKeysEagerly" in
    Map(1 → Map(2 → 3, 3 → 4), 2 → Map(3 → 4, 4 → 5)).nestedMap.mapKeysEagerly(_ * 2) ≡
      Map(1 → Map(4 → 3, 6 → 4), 2 → Map(6 → 4, 8 → 5))

  "nesteMap_mapEntries" in
    Map(1 → Map(2 → 3, 3 → 4), 2 → Map(3 → 4, 4 → 5)).nestedMap.mapEntries {
      case (outer, inner, value) ⇒ (outer + 1, inner - 1, value * 10)
    } ≡ Map(2 → Map(1 → 30, 2 → 40), 3 → Map(2 → 40, 3 → 50))

  "flipNesting" in Map(10 → Map(2 → 3, 3 → 4), 20 → Map(3 → 4, 4 → 5)).flipNesting ≡
    Map(2 → Map(10 → 3), 3 → Map(10 → 4, 20 → 4), 4 → Map(20 → 5))

  "append" in {
    on(Map(1 → Map(2 → 3))).calling(_.append(1, 2, 4), _.append(1, 3, 4), _.append(2, 3, 4)).produces(
      Map(1 → Map(2 → 4)), Map(1 → Map(2 → 3, 3 → 4)), Map(1 → Map(2 → 3), 2 → Map(3 → 4))
    )

    on(Map(1 → Map(2 → 3))).calling(_ + ((1, 2, 4)), _ + ((1, 3, 4)), _ + ((2, 3, 4))).produces(
      Map(1 → Map(2 → 4)), Map(1 → Map(2 → 3, 3 → 4)), Map(1 → Map(2 → 3), 2 → Map(3 → 4))
    )
  }

  "getOrEmpty" in
    on(Map(1 → Map(2 → 3))).calling(_.getOrEmpty(1), _.getOrEmpty(2)).produces(Map(2 → 3), Map())
}
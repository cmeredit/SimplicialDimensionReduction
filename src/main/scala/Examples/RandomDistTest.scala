package Examples

import DimensionReduction.DimensionStatUtil

object RandomDistTest extends App {
  println(DimensionStatUtil.getAverageMinDistanceInHypercube(4, 147, 1))
  println(DimensionStatUtil.getAverageMinDistanceInHypercube(4, 147, 10))
  println(DimensionStatUtil.getAverageMinDistanceInHypercube(4, 147, 100))
}

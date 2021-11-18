package DimensionReduction

object RandomDistStats {

  def getAverageMinDistanceInHypercube(dimension: Int, numPoints: Int, numRuns: Int = 10): Double = {

    val r = scala.util.Random

    def getRun: Double = {
      val uniformHypercubePoints: IndexedSeq[Point] = (0 until numPoints).map(_ => {
        Point((0 until dimension).map(_ => r.nextDouble()).toVector)
      })

      val minimumDistances: IndexedSeq[Double] = uniformHypercubePoints.map(p => {
        uniformHypercubePoints.flatMap(q => p.dist(q)).filter(_ > 0.0).min
      })

      minimumDistances.sum / minimumDistances.length.toDouble
    }

    val runs = (0 until numRuns).map(_ => getRun)

    runs.sum / runs.length.toDouble
  }

}

object RandomDistTest extends App {
  println(RandomDistStats.getAverageMinDistanceInHypercube(4, 147, 1))
  println(RandomDistStats.getAverageMinDistanceInHypercube(4, 147, 10))
  println(RandomDistStats.getAverageMinDistanceInHypercube(4, 147, 100))
}
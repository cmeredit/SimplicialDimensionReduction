package Examples

import DimensionReduction.{GeometricSimplicialComplex, Point}

// This set of tests currently passes. (Be warned - it takes a long time to run...)
object ProjectionTest extends App {
  val maybeCulledGeomComp: Option[GeometricSimplicialComplex] = GeometricSimplicialComplex.loadFromFile("Datasets/CulledNormalizedIrisComplex.txt")

  assert(maybeCulledGeomComp.nonEmpty)

  val culledGeomComp: GeometricSimplicialComplex = maybeCulledGeomComp.get

  val numPointsToTest: Int = 100
  val uniformHypercubePoints: Vector[Point] = (0 until numPointsToTest).map(_ => Point((0 until 4).map(_ => scala.util.Random.nextDouble()).toVector)).toVector

  uniformHypercubePoints.zipWithIndex.foreach({ case (point, idx) =>

    println("Testing point " + (idx + 1) + " of " + numPointsToTest)
    println(point)

    val projection: Point = culledGeomComp.project(point)
    val bruteForceProjection: Point = culledGeomComp.bruteForceProject(point)
    val projectionOfProjection: Point = culledGeomComp.project(projection)

    // A projection operator should be idempotent.
    assert(projection == projectionOfProjection)

    // Our intelligent operator should produce correct results
    assert(projection == bruteForceProjection)

    // Our complex should actually contain the projections of points onto the complex.
    assert(culledGeomComp.simplices.exists(_.containsPoint(projection)))
  })

}

package Examples

import DimensionReduction.Delaunay.PointedAffineSpace
import DimensionReduction.{Delaunay, GeometricSimplicialComplex, Point, Simplex}

object GeometricSimplicialComplexTest2 extends App {

  val points: Vector[Point] = Vector(
    Point(Vector(0.0, 0.0)),
    Point(Vector(3.0, 4.0)),
    Point(Vector(4.0, 3.0)),
    Point(Vector(0.0, 25.0 / 7.0)),
  )

  val k = 25.0 / 7.0

  val pointNames: Point => String = {
    case Point(Vector(0.0, 0.0)) => "O"
    case Point(Vector(3.0, 4.0)) => "Y"
    case Point(Vector(4.0, 3.0)) => "X"
    case Point(Vector(0.0, k)) => "S"
    case _ => "Unknown Point"
  }

  val delaunay: Vector[PointedAffineSpace] = Delaunay.DelaunayUtil.getDelaunaySimplicialization(points)

  delaunay.foreach((space: PointedAffineSpace) => {
    println("(" + space.vertices.map(pointNames).reduce(_ + ", " + _) + ")")
  })

  val simplicialization: GeometricSimplicialComplex = new GeometricSimplicialComplex(delaunay)

  println(simplicialization)

  simplicialization.nSimplices foreach { case (n, nSimplices) =>
    println(n + "-Simplices:")
    nSimplices.foreach((simplex: Simplex) =>
      println(if (simplex.points.nonEmpty) {
        "[" + simplex.points.map(pointNames).reduce(_ + ", " + _) + "]"
      } else "[]")
    )
  }

}

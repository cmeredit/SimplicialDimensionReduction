package Examples

import DimensionReduction.Delaunay.PointedAffineSpace
import DimensionReduction.{Delaunay, GeometricSimplicialComplex, Point, Simplex}

object GeometricSimplicialComplexTest extends App {

  val points: Vector[Point] = Vector(
    Point(Vector(0.0, 0.0, 0.0)),
    Point(Vector(1.0, 0.0, 0.0)),
    Point(Vector(0.0, 1.0, 0.0)),
    Point(Vector(0.0, 0.0, 1.0)),
    Point(Vector(1.0, 1.0, 1.0))
  )

  val pointNames: Point => String = {
    case Point(Vector(0.0, 0.0, 0.0)) => "O"
    case Point(Vector(1.0, 0.0, 0.0)) => "X"
    case Point(Vector(0.0, 1.0, 0.0)) => "Y"
    case Point(Vector(0.0, 0.0, 1.0)) => "Z"
    case Point(Vector(1.0, 1.0, 1.0)) => "S"
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

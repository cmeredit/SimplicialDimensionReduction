package Examples

import DimensionReduction.Delaunay.{DelaunayUtil, PointedAffineSpace, QuickHullUtil}
import DimensionReduction.Point

import scala.math.random

object DelaunayTest extends App {

  QuickHullUtil.DebugPrinter.shouldPrint = false

  val myPoints: Vector[Point] = (0 until 100).map(_ => Vector(random(), random())).toVector.map(Point)

  myPoints foreach { case Point(Vector(x, y)) => println("(" + x + ", " + y + ")") }

  val triangulation: Vector[PointedAffineSpace] = DelaunayUtil.getDelaunaySimplicialization(myPoints)

  triangulation foreach { (simplex: PointedAffineSpace) =>
    //    println("Simplex:")
    //    println(simplex.vertices)

    simplex.vertices.combinations(2).foreach({ case Vector(p1, p2) =>
      println("((1-t)*" + p1(0) + " + t*" + p2(0) + ", (1-t)*" + p1(1) + " + t*" + p2(1) + ")")
    })
  }

}

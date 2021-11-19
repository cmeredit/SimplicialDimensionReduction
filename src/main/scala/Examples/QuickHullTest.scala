package Examples

import DimensionReduction.Delaunay.QuickHullUtil.DebugPrinter
import DimensionReduction.Delaunay.{PointedAffineSpace, QuickHullUtil}
import DimensionReduction.Point

import scala.math.random

object QuickHullTest extends App {

  DebugPrinter.shouldPrint = false

  val oneDimPoints: Vector[Point] = (0 until 10).map(_ => Vector(random() * 2.0 - 1.0)).toVector.map(Point)
  val parabolicPoints = oneDimPoints.map(p => Point(p.coordinates ++ Vector(p.head * p.head)))

  val hullSimplices: Vector[PointedAffineSpace] = QuickHullUtil.getConvexHull(parabolicPoints) match {
    case Some(h) => h
    case None => Vector()
  }

  parabolicPoints.foreach(p =>
    println("(" + p.map(_.toString).reduce(_ + ", " + _) + ")")
  )

  hullSimplices.foreach((s: PointedAffineSpace) => {
    val startPoint: Point = s.vertices(0)
    val endPoint: Point = s.vertices(1)

    val x0: Double = startPoint(0)
    val y0: Double = startPoint(1)
    val x1: Double = endPoint(0)
    val y1: Double = endPoint(1)
    val dx: Double = x1 - x0
    val dy: Double = y1 - y0

    val niceToString: Double => String = (d: Double) => f"$d%1.4f"

    println("(" + niceToString(x0) + " + (" + niceToString(dx) + " * t)" + ", " + niceToString(y0) + " + (" + niceToString(dy) + " * t" + "))")


    // Also print the normal vector....
    val nx0 = (x0 + x1) / 2.0
    val ny0 = (y0 + y1) / 2.0

    val displayLength: Double = 0.5
    val ndx = s.normalVector(0) * displayLength
    val ndy = s.normalVector(1) * displayLength

    println("(" + niceToString(nx0) + " + (" + niceToString(ndx) + " * t)" + ", " + niceToString(ny0) + " + (" + niceToString(ndy) + " * t" + "))")


  })

}

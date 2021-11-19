package Examples

import DimensionReduction.{Point, Simplex}

object ProjTest extends App {

  val p0: Point = Point(Vector(0.0, 0.0))
  val p1: Point = Point(Vector(1.0, 1.0))

  val simplex: Simplex = Simplex(Set(p0, p1))

  assert(simplex.containsPoint(p0))
  assert(simplex.containsPoint(Point(Vector(0.5, 0.5))))
  assert(!simplex.containsPoint(Point(Vector(0.0, 1.0))))
  assert(!simplex.containsPoint(Point(Vector(2.0, 2.0))))


  println(simplex.getProjection(p0))
  println(simplex.getProjection(Point(Vector(0.5, 0.5))))
  println(simplex.getProjection(Point(Vector(0.0, 1.0))))
  println(simplex.getProjection(Point(Vector(2.0, 2.0))))


  println("Passed!")
}

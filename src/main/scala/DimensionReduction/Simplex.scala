package DimensionReduction

import DimensionReduction.Delaunay.{LinearUtil, PointedAffineSpace}
import spire.math.Rational

case class Simplex(points: Set[Point]) {

  def this(parentSpace: PointedAffineSpace) = this(parentSpace.vertices.toSet)

  private lazy val ptsVec: Vector[Point] = points.toVector

  private lazy val normalVec: Vector[Rational] = LinearUtil.getUnitNormalVector({
    val basePoint: Point = ptsVec.head
    val tailPoints: Vector[Point] = ptsVec.tail
    tailPoints.flatMap(_ - basePoint)
  }).coordinates.map(Rational(_))

  private lazy val localBasis: Vector[Vector[Rational]] = {
    val basePoint: Point = ptsVec.head
    val tailPoints: Vector[Point] = ptsVec.tail
    val differences: Vector[Point] = tailPoints.map(pt => (pt - basePoint).get)
    differences.map(pt => pt.coordinates.map(Rational(_)))
  }

  private lazy val changeOfBasisMatrix: Vector[Vector[Rational]] = LinearUtil.getInverse((LinearUtil.extendToBasis(localBasis)).transpose).get

  def containsPoint(p: Point, tolerance: Rational = Rational(0.0001)): Boolean = {

    val pColumnVec: Vector[Vector[Rational]] = Vector((p - basePoint).get.map(Rational(_))).transpose

    val pWRTThisBasisCol: Vector[Vector[Rational]] = LinearUtil.matrixMult(changeOfBasisMatrix, pColumnVec)

//    pWRTThisBasisCol foreach println

    val pWRTThisBasis: Vector[Rational] = pWRTThisBasisCol.transpose.head

//    println(pWRTThisBasis)

//    println("")
//    println("Conditions:")
//    println("Last entry:")
//    println(pWRTThisBasis.last.abs.toDouble)
//    println(pWRTThisBasis.last.abs < tolerance)
//    println("Point with respect to simplex localBasis:")
//    println(pWRTThisBasis.map(_.toDouble))
//    println(pWRTThisBasis.forall(_ >= Rational(0.0)))
//    println("Sum of coordinates:")
//    println(pWRTThisBasis.reduce(_ + _).abs.toDouble)
//    println(pWRTThisBasis.reduce(_ + _) < Rational(1.0) + tolerance)
//    println("")


    (pWRTThisBasis.last.abs < tolerance) && pWRTThisBasis.forall(_ >= Rational(0.0)) && (pWRTThisBasis.reduce(_ + _) < Rational(1.0) + tolerance)
  }

  // Projection matrix
  private lazy val P: Option[Vector[Vector[Rational]]] = LinearUtil.getProjectionMatrix(localBasis)

  private lazy val basePoint: Point = ptsVec.head

  def getProjection(p: Point): Option[Point] = {

    P match {
      case Some(projMatrix) =>
        val pColumnVec: Vector[Vector[Rational]] = Vector((p - basePoint).get.map(Rational(_))).transpose

        val projectionOntoHyperplane: Point = Point(LinearUtil.matrixMult(projMatrix, pColumnVec).transpose.head.map(_.toDouble))

        Some(projectionOntoHyperplane).filter(x => containsPoint(x))
      case None => Some(basePoint)
    }

  }

  override def toString: String = if (points.nonEmpty) {"[" + points.map(_.toString).reduce(_ + ", " + _) + "]"} else "[]"

}

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
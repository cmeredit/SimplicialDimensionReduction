package DimensionReduction

import DimensionReduction.Delaunay.PointedAffineSpace
import spire.math.Rational

case class Simplex(points: Set[Point]) {

  def this(parentSpace: PointedAffineSpace) = this(parentSpace.vertices.toSet)

  private lazy val ptsVec: Vector[Point] = points.toVector

  val dimension: Int = ptsVec.length - 1

//  private lazy val normalVec: Vector[Rational] = LinearUtil.getUnitNormalVector({
//    val basePoint: Point = ptsVec.head
//    val tailPoints: Vector[Point] = ptsVec.tail
//    tailPoints.flatMap(_ - basePoint)
//  }).coordinates.map(Rational(_))

  private lazy val localBasis: Vector[Vector[Rational]] = {
    val basePoint: Point = ptsVec.head
    val tailPoints: Vector[Point] = ptsVec.tail
    val differences: Vector[Point] = tailPoints.map(pt => (pt - basePoint).get)
    differences.map(pt => pt.coordinates.map(Rational(_)))
  }

  private lazy val localDimension: Int = localBasis.length
  private lazy val globalBasis: Vector[Vector[Rational]] = LinearUtil.extendToBasis(localBasis)

//  println("Points:")
//  ptsVec foreach println
//  println("Dimension: " + dimension)
//  println("Local basis:")
//  localBasis foreach println
//  println("Global basis:")
//  globalBasis foreach println

  private lazy val changeOfBasisMatrix: Vector[Vector[Rational]] = LinearUtil.getInverse(globalBasis.transpose).get

  def containsPoint(p: Point, tolerance: Rational = Rational(0.0001)): Boolean = {

    if (localDimension > 0 ) {
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

      val localCoordinatesOfP: Vector[Rational] = pWRTThisBasis.take(localDimension)
      val extraCoordinatesOfP: Vector[Rational] = pWRTThisBasis.drop(localDimension)

      extraCoordinatesOfP.forall(_.abs < tolerance) && localCoordinatesOfP.forall(_ >= Rational(0.0)) && (localCoordinatesOfP.reduce(_ + _) < Rational(1.0) + tolerance)
    } else {
      basePoint.dist(p) match {
        case None => false
        case Some(dist) =>
//          println("Distance: " + dist)
          dist < tolerance
      }
    }
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


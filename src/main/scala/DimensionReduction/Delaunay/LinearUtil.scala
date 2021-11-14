package DimensionReduction.Delaunay

import DimensionReduction.Point

/** Provides several Linear Algebra functions.
 *
 *  This object is meant to provide several quality of life functions for working with vectors. The most significant
 *  of which is [[LinearUtil.getSignedDistAndNormalToHyperplane]], which when given a collection of points that
 *  define a hyperplane of the appropriate dimension, returns the distance function associated to that hyperplane
 *  as well as a unit normal vector to that hyperplane.
 */
object LinearUtil {
  // Assume: "points" all lie on some hyperplane
  // Goal: Find a function that returns the signed distance from a point to ***A*** half space determined by that
  // hyperplane
  /** Returns the signed distance function and a normal vector to the hyperplane determined by the supplied points. */
  def getSignedDistAndNormalToHyperplane(points: Vector[Point], tolerance: Double = 0.0): (Point => Double, Point)  = {

    val basePoint: Point = points.head
    val basis: Vector[Point] = points.tail.map(p => Point(p.zip(basePoint).map(coordPair => coordPair._1 - coordPair._2)))
    val normalVector: Point = getUnitNormalVector(basis)

    (
      (v: Point) => {
      val offsetV: Point = Point(v.zip(basePoint).map(pair => pair._1 - pair._2))
      val distEstimate: Double = normalVector.zip(offsetV).map(pair => pair._1 * pair._2).sum
      if (scala.math.abs(distEstimate) < tolerance) 0.0 else distEstimate
      },
      normalVector
    )
  }

  /** normalizes the given vector. */
  def normalize(v: Point): Point = {
    val mag = scala.math.sqrt(v.map(c => c * c).sum)
    Point(v.map(_ / mag))
  }

  // Get a normal vector to the hyperplane with the given basis (does not guarantee anything about orientation)
  // THIS BETTER ACTUALLY BE A BASIS. IF IT'S NOT, THEN ANY ERRORS ARE YOUR FAULT
  /** Returns a vector that is normal to the hyperplane with the given basis. */
  def getUnitNormalVector(basis: Vector[Point]): Point = {
    // A basis must contain at least one vector.
    assert(basis.nonEmpty)
    // A basis cannot consist of dimension-zero vectors.
    assert(basis.head.nonEmpty)
    // A basis of a hyperplane of codimension 1 must contain a number of vectors equal to the dimension of the vectors
    // minus one
    assert(basis.length == basis.head.dimension - 1)
    // A basis must contain vectors of uniform dimension.
    assert(basis.map(_.dimension).distinct.length == 1)

    val rref: Vector[Vector[Double]] = getRREF(basis.map(_.coordinates))
    val pivotPositions: Vector[Int] = basis.indices.flatMap(rref(_).zipWithIndex.find(_._1 != 0.0).map(_._2)).toVector
    val nonpivotIndex: Int = basis.head.indices.filter(!pivotPositions.contains(_)).head
    val rrefTranspose: Vector[Vector[Double]] = rref.transpose
    val nonpivotColumn: Vector[Double] = rrefTranspose(nonpivotIndex)

    // Concerned about -0.0? Me too.
    val mostOfNormal: Vector[Double] = nonpivotColumn.map(entry => if (entry == 0.0) 0.0 else -1.0 * entry)

    normalize(Point(
      mostOfNormal.dropRight(mostOfNormal.length - nonpivotIndex) ++
      Vector(1.0) ++
      mostOfNormal.drop(nonpivotIndex)
    ))
  }

  /** Computes the row-reduced echelon form of the supplied matrix using Gaussian elimination. */
  def getRREF(rowMajorMatrix: Vector[Vector[Double]]): Vector[Vector[Double]] = {

    def forwardReduce(matrix: Vector[Vector[Double]]): Vector[Vector[Double]] = {
      // Find the first nonzero column
      val transpose: Vector[Vector[Double]] = matrix.transpose

      val pivotColumnIndex: Option[Int] = transpose.zipWithIndex.find({case (col, _) => col.exists(_ != 0.0)}).map(_._2)

      pivotColumnIndex match {
        case Some(idx) =>
          // Put the nonzero entry of the pivot column on top
          val reorderedMatrix: Vector[Vector[Double]] = matrix.sortBy(row => scala.math.abs(row(idx))).reverse

//          println("Reordered matrix...")
//          reorderedMatrix foreach println

          val pivotRow: Vector[Double] = reorderedMatrix.head
          val pivotEntry: Double = pivotRow(idx)

//          println("Pivot row...")
//          println(pivotRow)

          val eliminatedMatrixTail: Vector[Vector[Double]] = reorderedMatrix.tail.map((row: Vector[Double]) => {
            val scalar: Double = row(idx) / pivotEntry
            row.zip(pivotRow).zipWithIndex.map({case ((rowEntry, pivotEntry), curIdx) => if (curIdx == idx) 0.0 else rowEntry - scalar * pivotEntry})
          })

//          println("Result of this step...")
//          (Vector(pivotRow) ++ eliminatedMatrixTail) foreach println
//          println("")


          Vector(pivotRow) ++ forwardReduce(eliminatedMatrixTail)

        case None => matrix
      }
    }

    def rescaleByPivot(row: Vector[Double]): Vector[Double] = row.find(_ != 0.0) match {
      case Some(firstNonzeroEntry) => row.map(_ / firstNonzeroEntry)
      case None => row
    }

    def backSub(matrix: Vector[Vector[Double]]): Vector[Vector[Double]] = {
      if (matrix.isEmpty)
        matrix
      else {
        val lastRow: Vector[Double] = matrix.reverse.head
        val firstRows: Vector[Vector[Double]] = matrix.reverse.tail.reverse

        val pivotLocation: Option[Int] = lastRow.zipWithIndex.find(_._1 != 0.0).map(_._2)

        pivotLocation match {
          case Some(idx) =>
            val pivotValue = lastRow(idx)
            val firstRowsEliminated: Vector[Vector[Double]] = firstRows.map(row => row.zip(lastRow).map({case (rowEntry, pivotRowEntry) => rowEntry - pivotRowEntry * row(idx) / pivotValue}))
            if (firstRowsEliminated.nonEmpty)
              backSub(firstRowsEliminated) ++ Vector(lastRow)
            else
              matrix
          case
            None => backSub(firstRows) ++ Vector(lastRow)
        }
      }

    }

    val forwardReducedMatrix: Vector[Vector[Double]] = forwardReduce(rowMajorMatrix)
    backSub(forwardReducedMatrix.map(rescaleByPivot))
  }

  /** Tests if the given points lie on a hyperplane of codimension 1 */
  def doPointsDefineAHyperplaneOfCodimensionOne(points: Vector[Point]): Boolean = {
    if (points.isEmpty) // No points
      false
    else if (points.map(_.dimension).distinct.length != 1) // Nonuniform dimension
      false
    else {
      val dimension: Int = points.head.dimension

      if (points.length != dimension) // Need exactly [dimension] many points. E.g., 2 points in 2d space might define a line
        false
      else {
        val basePoint: Point = points.head
        val remainingPoints: Vector[Point] = points.tail

        val displacementVectors: Vector[Point] = remainingPoints.map((v: Point) =>
          Point(v.zip(basePoint).map({case (vCoord, basePointCoord) => vCoord-basePointCoord}))
        )

        val rref: Vector[Vector[Double]] = getRREF(displacementVectors.map(_.coordinates))

        // If the rref of displacement vectors contains a row of all zeros, then displacementVectors is linearly dependent, so we should return false...
        rref.find((row: Vector[Double]) => row.forall(_ == 0.0)) match {
          case Some(_) => false // There's a bad row! These points don't define a hyperplane of codimension 1
          case None => true // All rows are nonzero. We're good!
        }
      }
    }
  }
}

object LinearUtilTest extends App {
  val a: Point = Point(Vector(0.9051310475138654, 0.08442008172760171, -0.416666577286393))
  val b: Point = Point(Vector(0.9278894781448365, 0.03992321446481126, -0.3707118197399426))
  val c: Point = Point(Vector(0.9192682247682198, 0.13235814155761652, -0.3707118197399426))

  println((b-a).get.toString)
  println((c-a).get.toString)


  println(LinearUtil.getRREF(Vector((b-a).get, (c-a).get).map(_.coordinates)))

}
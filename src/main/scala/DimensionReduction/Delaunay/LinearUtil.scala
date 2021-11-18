package DimensionReduction.Delaunay

import DimensionReduction.Point
import spire.math.{Rational, RationalInstances}

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

    val rref: Vector[Vector[Rational]] = getRREF(basis.map(_.coordinates.map(Rational(_))))
    val pivotPositions: Vector[Int] = basis.indices.flatMap(rref(_).zipWithIndex.find(_._1 != 0.0).map(_._2)).toVector
    val nonpivotIndex: Int = basis.head.indices.filter(!pivotPositions.contains(_)).head
    val rrefTranspose: Vector[Vector[Rational]] = rref.transpose
    val nonpivotColumn: Vector[Rational] = rrefTranspose(nonpivotIndex)

    // Concerned about -0.0? Me too.
    val mostOfNormal: Vector[Double] = nonpivotColumn.map(entry => if (entry == Rational(0.0)) 0.0 else (Rational(-1.0) * entry).toDouble)

    normalize(Point(
      mostOfNormal.dropRight(mostOfNormal.length - nonpivotIndex) ++
      Vector(1.0) ++
      mostOfNormal.drop(nonpivotIndex)
    ))
  }

  /** Computes the row-reduced echelon form of the supplied matrix using Gaussian elimination. */
  def getRREF(rowMajorMatrix: Vector[Vector[Rational]], normalizeRows: Boolean = true): Vector[Vector[Rational]] = {

    def forwardReduce(matrix: Vector[Vector[Rational]]): Vector[Vector[Rational]] = {
      // Find the first nonzero column
      val transpose: Vector[Vector[Rational]] = matrix.transpose

      val pivotColumnIndex: Option[Int] = transpose.zipWithIndex.find({case (col, _) => col.exists(_ != Rational(0.0))}).map(_._2)

      pivotColumnIndex match {
        case Some(idx) =>
          // Put the nonzero entry of the pivot column on top
          val reorderedMatrix: Vector[Vector[Rational]] = matrix.sortBy(row => row(idx).abs).reverse

//          println("Reordered matrix...")
//          reorderedMatrix foreach println

          val pivotRow: Vector[Rational] = reorderedMatrix.head
          val pivotEntry: Rational = pivotRow(idx)

//          println("Pivot row...")
//          println(pivotRow)

          val eliminatedMatrixTail: Vector[Vector[Rational]] = reorderedMatrix.tail.map((row: Vector[Rational]) => {
            val scalar: Rational = row(idx) / pivotEntry
            row.zip(pivotRow).map({case (rowEntry, pivotEntry) => rowEntry - scalar * pivotEntry})
          })

//          println("Result of this step...")
//          (Vector(pivotRow) ++ eliminatedMatrixTail) foreach println
//          println("")


          Vector(pivotRow) ++ forwardReduce(eliminatedMatrixTail)

        case None => matrix
      }
    }

    def rescaleByPivot(row: Vector[Rational]): Vector[Rational] = row.find(_ != Rational(0.0)) match {
      case Some(firstNonzeroEntry) => row.map(_ / firstNonzeroEntry)
      case None => row
    }

    def backSub(matrix: Vector[Vector[Rational]]): Vector[Vector[Rational]] = {
      if (matrix.isEmpty)
        matrix
      else {
        val lastRow: Vector[Rational] = matrix.reverse.head
        val firstRows: Vector[Vector[Rational]] = matrix.reverse.tail.reverse

        val pivotLocation: Option[Int] = lastRow.zipWithIndex.find(_._1 != 0.0).map(_._2)

        pivotLocation match {
          case Some(idx) =>
            val pivotValue = lastRow(idx)
            val firstRowsEliminated: Vector[Vector[Rational]] = firstRows.map(row => row.zip(lastRow).map({case (rowEntry, pivotRowEntry) => rowEntry - pivotRowEntry * row(idx) / pivotValue}))
            if (firstRowsEliminated.nonEmpty)
              backSub(firstRowsEliminated) ++ Vector(lastRow)
            else
              matrix
          case
            None => backSub(firstRows) ++ Vector(lastRow)
        }
      }

    }

    val forwardReducedMatrix: Vector[Vector[Rational]] = forwardReduce(rowMajorMatrix)

    if (normalizeRows) backSub(forwardReducedMatrix.map(rescaleByPivot)) else backSub(forwardReducedMatrix)
  }

  def getAbsoluteDeterminant(matrix: Vector[Vector[Rational]]): Rational = {
    assert(matrix.nonEmpty)
    assert(matrix.head.nonEmpty)
    assert(matrix.head.length == matrix.length)
    assert(matrix.distinctBy(_.length).distinct.length == 1)

    val unscaledRREF: Vector[Vector[Rational]] = getRREF(matrix, normalizeRows = false)

    val diagonalEntries: Vector[Rational] = unscaledRREF.zipWithIndex.map({case (row, n) => row(n)})

    diagonalEntries.reduce(_ * _).abs
  }

  def getInverse(matrix: Vector[Vector[Rational]]): Option[Vector[Vector[Rational]]] = {
    assert(matrix.nonEmpty)
    assert(matrix.head.nonEmpty)
    assert(matrix.head.length == matrix.length)
    assert(matrix.distinctBy(_.length).distinct.length == 1)

    val absoluteDeterminant = getAbsoluteDeterminant(matrix)

    if (absoluteDeterminant == Rational(0.0)) {
      None
    } else {

      val dimension: Int = matrix.length

      val identityMatrix: Vector[Vector[Rational]] = (0 until dimension).map(n => {
        Vector.fill(n)(Rational(0.0)) ++ Vector(Rational(1.0)) ++ Vector.fill(dimension-n-1 max 0)(Rational(0.0))
      }).toVector

      val concatenatedMatrices: Vector[Vector[Rational]] = matrix.zip(identityMatrix).map({case (matRow, identRow) => matRow ++ identRow})

      val concatenatedRREF: Vector[Vector[Rational]] = getRREF(concatenatedMatrices)

      val inverse: Vector[Vector[Rational]] = concatenatedRREF.map(_.drop(dimension))

      Some(inverse)

    }
  }

  def matrixMult(m1: Vector[Vector[Rational]], m2: Vector[Vector[Rational]]): Vector[Vector[Rational]] = {

    val m2Transpose: Vector[Vector[Rational]] = m2.transpose

    val m1Rows: Int = m1.length
    val m2Cols: Int = m2Transpose.length

    assert(m1Rows == m2Cols)

    val prod: Vector[Vector[Rational]] = (0 until m1Rows).map(i => {(0 until m1Rows).map(j => {
      m1(i).zip(m2Transpose(j)).map({case (a, b) => a * b}).reduce(_ + _)
    }).toVector}).toVector

    prod

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

        val rref: Vector[Vector[Rational]] = getRREF(displacementVectors.map(_.coordinates.map(Rational(_))))

        // If the rref of displacement vectors contains a row of all zeros, then displacementVectors is linearly dependent, so we should return false...
        rref.find((row: Vector[Rational]) => row.forall(_ == 0.0)) match {
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


  println(LinearUtil.getRREF(Vector((b-a).get.map(Rational(_)), (c-a).get.map(Rational(_)))))

}

object InverseTest extends App {

  val m: Vector[Vector[Rational]] = Vector(
    Vector(Rational(1.0), Rational(2.0), Rational(3.0)),
    Vector(Rational(6.0), Rational(5.0), Rational(4.0)),
    Vector(Rational(1.0), Rational(0.0), Rational(0.0))
  )

  LinearUtil.getInverse(m) match {
    case Some(inverse) => inverse foreach println
    case None => println("Uh oh! Matrix wasn't computed...")
  }

}

object MatrixMultTest extends App {
  val m: Vector[Vector[Rational]] = Vector(
    Vector(Rational(1.0), Rational(2.0), Rational(3.0)),
    Vector(Rational(6.0), Rational(5.0), Rational(4.0)),
    Vector(Rational(1.0), Rational(0.0), Rational(0.0))
  )

  LinearUtil.getInverse(m) match {
    case Some(inverse) => LinearUtil.matrixMult(m, inverse) foreach println
    case None => println("Uh oh! Matrix wasn't computed...")
  }
}
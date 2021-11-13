package DimensionReduction.Delaunay

object LinearUtil {
  // Assume: "points" all lie on some hyperplane
  // Goal: Find a function that returns the signed distance from a point to ***A*** half space determined by that
  // hyperplane
  def getSignedDistanceFunctionToHyperplane(points: Vector[Vector[Double]], tolerance: Double = 0.0): (Vector[Double] => Double, Vector[Double])  = {

    val basePoint: Vector[Double] = points.head
    val basis: Vector[Vector[Double]] = points.tail.map(p => p.zip(basePoint).map(coordPair => coordPair._1 - coordPair._2))

    val normalVector: Vector[Double] = getUnitNormalVector(basis)

//    println("Base point:")
//    println(basePoint)
//    println("Basis:")
//    println(basis)
//    println("Normal vector:")
//    println(normalVector)

    ((v: Vector[Double]) => {
      val offsetV: Vector[Double] = v.zip(basePoint).map(pair => pair._1 - pair._2)
      val distEstimate: Double = normalVector.zip(offsetV).map(pair => pair._1 * pair._2).sum
      if (scala.math.abs(distEstimate) < tolerance) 0.0 else distEstimate
    }, normalVector)
  }

  def normalize(v: Vector[Double]): Vector[Double] = {
    val mag = scala.math.sqrt(v.map(c => c * c).sum)
    v.map(_ / mag)
  }

  // Get a normal vector to the hyperplane with the given basis (does not guarantee anything about orientation)
  // THIS BETTER ACTUALLY BE A BASIS. IF IT'S NOT, THEN ANY ERRORS ARE YOUR FAULT
  def getUnitNormalVector(basis: Vector[Vector[Double]]): Vector[Double] = {

//    println(basis)

    assert(basis.nonEmpty)
    assert(basis.head.nonEmpty)
    assert(basis.length == basis.head.length - 1)

    val rref: Vector[Vector[Double]] = getRREF(basis)

//    println("RREF")
//    rref foreach println

    val pivotPositions: Vector[Int] = basis.indices.flatMap(rref(_).zipWithIndex.find(_._1 != 0.0).map(_._2)).toVector

//    println("Pivot positions")
//    println(pivotPositions)

    val nonpivotIndex: Int = basis.head.indices.filter(!pivotPositions.contains(_)).head

//    println("Nonpivot Index")
//    println(nonpivotIndex)

    val rrefTranspose: Vector[Vector[Double]] = rref.transpose
    val nonpivotColumn: Vector[Double] = rrefTranspose(nonpivotIndex)

    val mostOfNormal: Vector[Double] = nonpivotColumn.map(entry => if (entry == 0.0) 0.0 else -1.0 * entry)

    normalize(mostOfNormal.dropRight(mostOfNormal.length - nonpivotIndex) ++ Vector(1.0) ++ mostOfNormal.drop(nonpivotIndex))
  }

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

  def doPointsDefineAHyperplaneOfCodimensionOne(points: Vector[Vector[Double]]): Boolean = {
    if (points.isEmpty) // No points
      false
    else if (points.map(_.length).distinct.length != 1) // Nonuniform dimension
      false
    else {
      val dimension: Int = points.head.length

      if (points.length != dimension) // Need exactly [dimension] many points. E.g., 2 points in 2d space might define a line
        false
      else {
        val basePoint: Vector[Double] = points.head
        val remainingPoints: Vector[Vector[Double]] = points.tail

        val displacementVectors: Vector[Vector[Double]] = remainingPoints.map((v: Vector[Double]) => v.zip(basePoint).map({case (vCoord, basePointCoord) => vCoord-basePointCoord}))

        val rref: Vector[Vector[Double]] = getRREF(displacementVectors)

        // If the rref of displacement vectors contains a row of all zeros, then displacementVectors is linearly dependent, so we should return false...
        rref.find((row: Vector[Double]) => row.forall(_ == 0.0)) match {
          case Some(_) => false // There's a bad row! These points don't define a hyperplane of codimension 1
          case None => true // All rows are nonzero. We're good!
        }
      }
    }
  }
}

//object Test extends App {
////  LinearUtil.getRREF(Vector(
////    Vector(0.0, 3.0, -6.0, 6.0, 4.0, -5.0),
////    Vector(3.0, -7.0, 8.0, -5.0, 8.0, 9.0),
////    Vector(3.0, -9.0, 12.0, -9.0, 6.0, 15.0)
////  )) foreach println
//
//
//  println(LinearUtil.getUnitNormalVector(Vector(Vector(1.0, 1.0))))
//  println(LinearUtil.getUnitNormalVector(Vector(Vector(1.0, 1.0, 0.0), Vector(0.0, 0.0, 1.0))))
//
//
//  val dfunc = LinearUtil.getSignedDistanceFunctionToHyperplane(Vector(
//    Vector(1.0, 1.0, 1.0),
//    Vector(2.0, 1.0, 1.0),
//    Vector(1.0, 2.0, 1.0)
//  ))
//
//  println(dfunc(Vector(1.0, 1.0, 1.0)))
//  println(dfunc(Vector(0.0, 0.0, 1.0)))
//  println(dfunc(Vector(0.0, 0.0, 10.0)))
//
//}


object LinearUtilTest extends App {
  val a: Vector[Double] = Vector(0.9051310475138654, 0.08442008172760171, -0.416666577286393)
  val b: Vector[Double] = Vector(0.9278894781448365, 0.03992321446481126, -0.3707118197399426)
  val c: Vector[Double] = Vector(0.9192682247682198, 0.13235814155761652, -0.3707118197399426)


  def diff(x: Vector[Double], y: Vector[Double]): Vector[Double] = x.zip(y).map({case (s, t) => s-t})
  def str(x: Vector[Double]): String = "(" + x.map(_.toString).reduce(_ + ", " + _) + ")"

  println(str(diff(b, a)))
  println(str(diff(c, a)))


  println(LinearUtil.getRREF(Vector(diff(b, a), diff(c, a))))

}



package GLUtil

object VectorUtil {

  // Transformation defined by rotating u toward v by angle theta (radians)
  def getRotationFunction(u: Vector[Double], v: Vector[Double], theta: Double): Vector[Double] => Vector[Double] = {

    assert(u.length == v.length)
    val dim: Int = u.length

    val sinTheta: Double = scala.math.sin(theta)
    val cosTheta: Double = scala.math.cos(theta)

    // Use Rodrigues' rotation formula (based on generating function vu^T-uv^T)
    def rotationMatrixEntry(i: Int, j: Int): Double = {
      val idEntry: Double = if (i == j) 1.0 else 0.0
      val genEntry: Double = (v(i) * u(j) - u(i)*v(j)) * sinTheta
      val genSquaredEntry: Double = (u(i) * u(j) + v(i)*v(j)) * (cosTheta - 1.0)

      idEntry + genEntry + genSquaredEntry
    }

    (w: Vector[Double]) => {
      (0 until dim).map((i: Int) => (0 until dim).map((j: Int) => {
        w(j) * rotationMatrixEntry(i, j)
      }).sum).toVector
    }
  }

  def getProjectionFunction(basis: Vector[Vector[Double]]): Vector[Double] => Vector[Double] = {

    assert(basis.nonEmpty)
    assert(basis.map(_.length).distinct.length == 1)
    assert(basis.forall(_.sum != 0))

    (u: Vector[Double]) => {
      val coefficients: Vector[Double] = basis.map((v: Vector[Double]) => {
        val uDotV: Double = u.zip(v).map((pair: (Double, Double)) => pair._1 * pair._2).sum
        val vDotV: Double = v.zip(v).map((pair: (Double, Double)) => pair._1 * pair._2).sum
        uDotV / vDotV
      })

      val scaledBasis: Vector[Vector[Double]] = coefficients.zip(basis).map({case (coeff, vec) => vec.map(coeff * _)})

      scaledBasis.transpose.map(_.sum)

    }
  }
}

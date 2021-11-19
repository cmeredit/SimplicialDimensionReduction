package Examples

import DimensionReduction.LinearUtil
import spire.math.Rational

object ProjectionTest2 extends App {

  val basis: Vector[Vector[Rational]] = Vector(Vector(Rational(2.0), Rational(1.0)))

  val P = LinearUtil.getProjectionMatrix(basis)

  P foreach println

  LinearUtil.matrixMult(P.get, basis.transpose) foreach println

  LinearUtil.matrixMult(P.get, Vector(Vector(Rational(1.0), Rational(-2.0))).transpose) foreach println

}

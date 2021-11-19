package Examples

import DimensionReduction.LinearUtil
import spire.math.Rational

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

package ScalaComp

import scala.math.sqrt

class Point(val coordinates: List[Double]) {
  val dimension: Int = coordinates.length

  private def applyCoordinatewise(other: Point)(op: (Double, Double) => Double): Option[Point] =
    if (dimension == other.dimension) {
      Some(
        new Point(
          coordinates.zip(other.coordinates).map(p => op(p._1, p._2))
        )
      )
    } else {
      None
    }
  private def applyCoordinatewise(op: Double => Double): Point = new Point(coordinates.map(op))

  def +(other: Point): Option[Point] = applyCoordinatewise(other)(_ + _)

  def *(s: Double): Point = applyCoordinatewise(_ * s)
  def *(other: Point): Option[Point] = applyCoordinatewise(other)(_ * _)

  def -(other: Point): Option[Point] = this + (other * -1.0)

  def distSquared(other: Point): Option[Double] = {
    val difference: Option[Point] = this - other
    val differenceSquared: Option[Point] = difference.flatMap(p => p * p)
    differenceSquared.map(_.coordinates.sum)
  }

  def dist(other: Point): Option[Double] = distSquared(other).map(sqrt)

  override def toString: String =
    "Point(Coordinates: " + coordinates.toString() + ", Dimension: " + dimension.toString
}

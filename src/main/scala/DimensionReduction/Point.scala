package DimensionReduction

import scala.math.sqrt

/** Represents a point in space of arbitrary dimension.
 *
 *  The primary purpose of this class is to augment Vector[Double] with helper functions and attributes.
 *  Operations are generally only well defined for points of the same dimension. Most functions in this
 *  class therefore return Option[Point]
 *
 *  @param coordinates The coordinates of the point.
 */
case class Point(coordinates: Vector[Double]) {
  /** The dimension of the point. Equal to the length of the coordinates vector. */
  val dimension: Int = coordinates.length

  def apply(i: Int): Double = coordinates(i)
  def head: Double = coordinates.head
  def zip(other: Point): Vector[(Double, Double)] = coordinates.zip(other.coordinates)
  def zipWithIndex: Vector[(Double, Int)] = coordinates.zipWithIndex
  def map[B](op: Double => B): Vector[B] = coordinates.map(op)
  def nonEmpty: Boolean = coordinates.nonEmpty
  def find(p: Double => Boolean): Option[Double] = coordinates.find(p)
  def indices: Range = coordinates.indices

  private def applyCoordinatewise(other: Point)(op: (Double, Double) => Double): Option[Point] =
    if (dimension == other.dimension) {
      Some(
        Point(coordinates.zip(other.coordinates).map(p => op(p._1, p._2)))
      )
    } else {
      None
    }
  private def applyCoordinatewise(op: Double => Double): Point = Point(coordinates.map(op))

  /** (Optionally) Returns the vector sum of this with other */
  def +(other: Point): Option[Point] = applyCoordinatewise(other)(_ + _)

  /** Returns the scalar product of s with this */
  def *(s: Double): Point = applyCoordinatewise(_ * s)
  /** (Optionally) Returns the coordinatewise product of this with other */
  def *(other: Point): Option[Point] = applyCoordinatewise(other)(_ * _)

  /** (Optionally) Returns the vector difference of this with other */
  def -(other: Point): Option[Point] = this + (other * -1.0)

  /** (Optionally) Returns the square of the distance between this and other */
  def distSquared(other: Point): Option[Double] = {
    val difference: Option[Point] = this - other
    val differenceSquared: Option[Point] = difference.flatMap(p => p * p)
    differenceSquared.map(_.coordinates.sum)
  }

  /** (Optionally) Returns the distance between this and other */
  def dist(other: Point): Option[Double] = distSquared(other).map(sqrt)

  override def toString: String =
    "Point(Coordinates: " + coordinates.toString() + ", Dimension: " + dimension.toString
}

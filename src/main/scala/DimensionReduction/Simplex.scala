package DimensionReduction

import DimensionReduction.Delaunay.PointedAffineSpace

case class Simplex(points: Set[Point]) {

  def this(parentSpace: PointedAffineSpace) = this(parentSpace.vertices.toSet)

  override def toString: String = if (points.nonEmpty) {"[" + points.map(_.toString).reduce(_ + ", " + _) + "]"} else "[]"

}

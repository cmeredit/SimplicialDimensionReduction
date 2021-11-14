package DimensionReduction

import DimensionReduction.Delaunay.PointedAffineSpace

class Simplex(val points: Vector[Point]) {

  def this(parentSpace: PointedAffineSpace) = this(parentSpace.vertices)

}

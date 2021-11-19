package DimensionReduction.Delaunay

import DimensionReduction.Point

/** Represents an affine space of arbitrary dimension with a full subset of points.
 *
 *  A pointed affine subspace can be represented as a collection of vertices. We augment this representation with a distance function
 *  and a normal vector (which should be compatible with the distance function, but is not forced to be).
 *
 * @param vertices An affinely-independent set of points of the space.
 * @param signedDistance The signed distance to the space.
 * @param normalVector A normal vector to the space.
 */
case class PointedAffineSpace(vertices: Vector[Point], signedDistance: Point => Double, normalVector: Point)

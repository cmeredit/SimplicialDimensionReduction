package DimensionReduction.Delaunay

/** Represents a Simplex of arbitrary dimension.
 *
 *  A simplex can be represented as a collection of vertices. We augment this representation with a distance function
 *  (in [[QuickHullUtil]] and [[DelaunayUtil]], this function represents the distance to the unique subspace that
 *  contains the simplex and has the same dimension as the simplex) and a normal vector (which should be compatible
 *  with the distance function, but is not forced to be).
 *
 * @param vertices The vertices of the simplex.
 * @param signedDistance The distance to the simplex or its parent hyperplane.
 * @param normalVector A normal vector to the simplex.
 */
case class Simplex(vertices: Vector[Vector[Double]], signedDistance: Vector[Double] => Double, normalVector: Vector[Double])

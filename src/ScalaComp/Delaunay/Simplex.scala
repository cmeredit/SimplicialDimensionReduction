package ScalaComp.Delaunay

case class Simplex(vertices: Vector[Vector[Double]], signedDistance: Vector[Double] => Double, normalVector: Vector[Double])

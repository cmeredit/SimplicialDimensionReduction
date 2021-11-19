package DimensionReduction.Delaunay

import DimensionReduction.{LinearUtil, Point}

import scala.annotation.tailrec
import scala.math.random

/** Provides functions to compute the Delaunay simplicialization of a collection of points of uniform dimension.
 *
 *  In two dimensions, the Delaunay simplicialization is the usual Delaunay triangulation. I.e., a triangulation of the
 *  points such that no circumcircle of any triangle contains another point in its interior.
 *
 *  In three dimensions, the Delaunay simplicialization of a set of points is a collection of tetrahedra such that no
 *  circumsphere of any tetrahedron contains another point in its interior.
 *
 *  In n dimensions, the Delaunay simplicialization of a set of points is a collection of n-simplices such that no
 *  circumhypersphere of any n-simplex contains another point in its interior.
 *
 */
object DelaunayUtil {

  /** Computes the Delaunay simplicialization of a set of points.
   *
   *  The currently implementation of this code computes a lift of the points to the unit hyperparaboloid, computes
   *  the convex hull of the lift, and then projects this hull back to the original space.
   *
   *  In theory, the expected running time of this algorithm is O(n log(n)), but because I've incorporated some slow
   *  extra algorithms (e.g., naive Gaussian elimination), the running time is probably worse.
   *
   * @param points The points to be represented
   * @return The Delaunay simplicialization of the supplied points.
   */
  def getDelaunaySimplicialization(points: Vector[Point]): Vector[PointedAffineSpace] = {

    assert(points.nonEmpty)
    assert(points.head.nonEmpty)
    assert(points.distinctBy(_.dimension).length == 1)

    val dimension = points.head.dimension

    val maximumMagnitudeSquared: Double = points.map((p: Point) => p.zip(p).map({case (a, b) => a*b}).sum).max
    val veryLowVector: Point = Point(Vector.fill(dimension)(0.0) ++ Vector(-1.0 * maximumMagnitudeSquared - 1.0))
//    println(veryLowVector)

    // Lift points to higher dim
    val liftedPoints: Vector[Point] = points.map(p => Point(p.coordinates ++ Vector(p.zip(p).map({case (a, b) => a * b * 0.1}).sum)))

//    val convexHullOfLift: Vector[PointedAffineSpace] = {
//
//      var currentHull = QuickHullUtil.getConvexHull(liftedPoints)
//      var hullSize: Int = 0
//
//      do {
//        hullSize = currentHull.flatMap(_.vertices).distinct.length
//        currentHull = QuickHullUtil.getConvexHull(currentHull.flatMap(_.vertices).distinct)
//      } while (hullSize != currentHull.flatMap(_.vertices).distinct.length)
//
//      currentHull
//    }

    @tailrec
    def lexographicLessThan(p: Point, q: Point): Boolean = {
      if (p.head == q.head)
        if (p.nonEmpty && q.nonEmpty)
          lexographicLessThan(p.tail, q.tail)
        else false
      else
        p.head < q.head
    }

    val convexHullOfLift: Vector[PointedAffineSpace] = QuickHullUtil.getConvexHull(liftedPoints) match {
      case Some(nondegenerateHull) => nondegenerateHull
      case None =>
        val lexographicOrderedPoints: Vector[Point] = liftedPoints.sortWith(lexographicLessThan)

        lexographicOrderedPoints.sliding(dimension+1).map(coords => PointedAffineSpace(coords, _ => 0.0, Point(Vector(-1.0)))).toVector
    }


//    val lowerEnvelopeOfLiftedHull: Vector[Simplex] = convexHullOfLift.filter((simplex: Simplex) => simplex.signedDistance(veryLowVector) >= 0.0)

//    val lowerEnvelopeOfLiftedHull: Vector[Simplex] = convexHullOfLift.filter((simplex: Simplex) => simplex.signedDistance(simplex.vertices.head.zip(veryLowVector).map({case (a, b) => a + b})) >= 0.0)

//    println("Normal vectors...")
    val lowerEnvelopeOfLiftedHull: Vector[PointedAffineSpace] = convexHullOfLift.filter((simplex: PointedAffineSpace) => {
//      println(simplex.normalVector)
      simplex.normalVector.coordinates.last <= 0.0
//      simplex.signedDistance(veryLowVector) >= 0.0
    })

//    println("Lower envelope")
//    lowerEnvelopeOfLiftedHull foreach println

    val projectionOfLowerEnvelope: Vector[PointedAffineSpace] = lowerEnvelopeOfLiftedHull.map((simplex: PointedAffineSpace) => {

//      val trueVertices: Vector[Point] = simplex.vertices.map(p => Point(p.coordinates.reverse.tail.reverse))
//
////      println("True vertices:")
////      println(trueVertices)
////      println("Num true vertices:")
////      println(trueVertices.length)
//
//      trueVertices.combinations(simplex.vertices.length - 1).map((smallSubsetOfVertices: Vector[Point]) => {
//        val absDist: Point => Double = (v: Point) => scala.math.abs(LinearUtil.getSignedDistAndNormalToHyperplane(smallSubsetOfVertices)._1(v))
//        val normal: Point = LinearUtil.getSignedDistAndNormalToHyperplane(smallSubsetOfVertices)._2
//
//        PointedAffineSpace(smallSubsetOfVertices, absDist, normal)
//      }).distinctBy(_.vertices.toSet)

      val newPoints: Vector[Point] = simplex.vertices.map(_.droppedLast)
      val basis: Vector[Point] = newPoints.tail.flatMap((pt: Point) => pt - newPoints.head)
      val absDist: Point => Double = (v: Point) => scala.math.abs(LinearUtil.getSignedDistAndNormalToHyperplane(basis)._1(v))
      val normal: Point = LinearUtil.getSignedDistAndNormalToHyperplane(basis)._2

      PointedAffineSpace(newPoints, absDist, normal)

    })

    projectionOfLowerEnvelope

  }

}

object DelaunayTest extends App {

  QuickHullUtil.DebugPrinter.shouldPrint = false

  val myPoints: Vector[Point] = (0 until 100).map(_ => Vector(random(), random())).toVector.map(Point)

  myPoints foreach {case Point(Vector(x, y)) => println("(" + x + ", " + y + ")")}

  val triangulation: Vector[PointedAffineSpace] = DelaunayUtil.getDelaunaySimplicialization(myPoints)

  triangulation foreach {(simplex: PointedAffineSpace) =>
//    println("Simplex:")
//    println(simplex.vertices)

    simplex.vertices.combinations(2).foreach({case Vector(p1, p2) =>
      println("((1-t)*" + p1(0) + " + t*" + p2(0) + ", (1-t)*" + p1(1) + " + t*" + p2(1) + ")")
    })
  }

}

package DimensionReduction.Delaunay

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
   *  extra algorithms (e.g., naiive Gaussian elimination), the running time is probably worse.
   *
   * @param points The points to be represented
   * @return The Delaunay simplicialization of the supplied points.
   */
  def getDelaunaySimplicialization(points: Vector[Vector[Double]]): Vector[Simplex] = {

    assert(points.nonEmpty)
    assert(points.head.nonEmpty)
    assert(points.distinctBy(_.length).length == 1)

    val dimension = points.head.length

    val maximumMagnitudeSquared: Double = points.map((p: Vector[Double]) => p.zip(p).map({case (a, b) => a*b}).sum).max
    val veryLowVector: Vector[Double] = Vector.fill(dimension)(0.0) ++ Vector(-1.0 * maximumMagnitudeSquared - 1.0)
//    println(veryLowVector)

    // Lift points to higher dim
    val liftedPoints: Vector[Vector[Double]] = points.map(p => p ++ Vector(p.zip(p).map({case (a, b) => a * b * 0.0001}).sum))

    val convexHullOfLift: Vector[Simplex] = {

      var currentHull = QuickHullUtil.getConvexHull(liftedPoints)
      var hullSize: Int = 0

      do {
        hullSize = currentHull.flatMap(_.vertices).distinct.length
        currentHull = QuickHullUtil.getConvexHull(currentHull.flatMap(_.vertices).distinct)
      } while (hullSize != currentHull.flatMap(_.vertices).distinct.length)

      currentHull
    }

//    println("Convex hull of lift")
//    convexHullOfLift foreach {(simplex: Simplex) =>
//      println(simplex.vertices.toString() + " has distance " + simplex.signedDistance({
//        val projectionOfVertex = simplex.vertices.head.reverse.tail.reverse ++ Vector(0.0)
//        projectionOfVertex
//      }) + " to " + {
//        val projectionOfVertex = simplex.vertices.head.reverse.tail.reverse ++ Vector(0.0)
//        projectionOfVertex
//      })
//    }

//    val lowerEnvelopeOfLiftedHull: Vector[Simplex] = convexHullOfLift.filter((simplex: Simplex) => simplex.signedDistance(veryLowVector) >= 0.0)

//    val lowerEnvelopeOfLiftedHull: Vector[Simplex] = convexHullOfLift.filter((simplex: Simplex) => simplex.signedDistance(simplex.vertices.head.zip(veryLowVector).map({case (a, b) => a + b})) >= 0.0)

//    println("Normal vectors...")
    val lowerEnvelopeOfLiftedHull: Vector[Simplex] = convexHullOfLift.filter((simplex: Simplex) => {
//      println(simplex.normalVector)
//      simplex.normalVector.last < 0.0
      simplex.signedDistance(veryLowVector) >= 0.0
    })

//    println("Lower envelope")
//    lowerEnvelopeOfLiftedHull foreach println

    val projectionOfLowerEnvelope: Vector[Simplex] = lowerEnvelopeOfLiftedHull.flatMap((simplex: Simplex) => {

      val trueVertices: Vector[Vector[Double]] = simplex.vertices.map(_.reverse.tail.reverse)

//      println("True vertices:")
//      println(trueVertices)
//      println("Num true vertices:")
//      println(trueVertices.length)

      trueVertices.combinations(simplex.vertices.length - 1).map((smallSubsetOfVertices: Vector[Vector[Double]]) => {
        val absDist: Vector[Double] => Double = (v: Vector[Double]) => scala.math.abs(LinearUtil.getSignedDistAndNormalToHyperplane(smallSubsetOfVertices)._1(v))
        val normal: Vector[Double] = LinearUtil.getSignedDistAndNormalToHyperplane(smallSubsetOfVertices)._2

        Simplex(smallSubsetOfVertices, absDist, normal)
      }).distinctBy(_.vertices.toSet)

    })

    projectionOfLowerEnvelope

  }

}

object DelaunayTest extends App {

  QuickHullUtil.DebugPrinter.shouldPrint = false

  val myPoints: Vector[Vector[Double]] = (0 until 100).map(_ => Vector(random(), random())).toVector

  myPoints foreach {case Vector(x, y) => println("(" + x + ", " + y + ")")}

  val triangulation: Vector[Simplex] = DelaunayUtil.getDelaunaySimplicialization(myPoints)

  triangulation foreach {(simplex: Simplex) =>
//    println("Simplex:")
//    println(simplex.vertices)

    simplex.vertices.combinations(2).foreach({case Vector(p1, p2) =>
      println("((1-t)*" + p1(0) + " + t*" + p2(0) + ", (1-t)*" + p1(1) + " + t*" + p2(1) + ")")
    })
  }

}

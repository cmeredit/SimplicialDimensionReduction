package DimensionReduction

import DimensionReduction.Delaunay.PointedAffineSpace

import scala.io.{BufferedSource, Source}

/** The geometric realization of a simplicial set.
 *
 *  The default constructor builds a simplicial complex from a set of pointed affine spaces. There is no
 *  guarantee that a simplicial complex only contains nondegenerate simplices - buyer beware.
 *  The optional parameter, forcedNSimplices, is used internally when pruning the simplicial complex. It
 *  generally should not be used externally since there are no checks in place to guarantee that forcedNSimplices
 *  actually defines a valid simplicial complex.
 *
 *  A simplicial set is a contravariant functor from the simplicial category Δ to the category of sets.
 *  The connection of simplicial sets to geometric simplicial complexes is explained below.
 *
 *  Let F: Δ -> SET be a simplicial set. Then:
 *
 *  We consider F([n]) to be the set of n-simplices of the complex.
 *
 *  The simplicial category contains two important types of functions: Face and degeneracy maps.
 *
 *  The (n, i)-face map in Δ is the unique nondecreasing function δ: [n-1] -> [n] that does not have i in its image.
 *  F(δ): F([n]) -> F([n-1]) is interpreted as the function that maps an n-simplex to its i-th face of dimension n-1.
 *
 *  The (n, i)-degeneracy map in Δ is the unique nondecreasing function σ: [n+1] -> [n] whose preimage of i has size 2.
 *  Degeneracy maps are in some since inverse to face maps:
 *  F(σ): F([n]) -> F([n+1]) is interpreted as the function that maps an n-simplex to its i-th degenerate simplex.
 *  The degeneracy maps should not be computationally useful. If they are, please alert me and I'll implement them.
 *
 */
class GeometricSimplicialComplex(componentSpaces: Vector[PointedAffineSpace],
                                 forcedNSimplices: Option[Map[Int,Vector[Simplex]]] = None,
                                 forcedNFaceMap: Option[Map[(Int, Simplex),Vector[Simplex]]] = None) {

  /** The highest inherent dimension of any simplex in the complex.
   *  This is also the lowest dimension of a Euclidean space into which the complex can be embedded
   */
  val maxComponentDimension: Int = forcedNSimplices match {
    case Some(forced) => forced.keys.max
    case None => componentSpaces.map(_.vertices.length).max - 1
  }

  // Start by converting the component spaces into simplices and group them according to their dimension.
  // The component spaces might have the following problems:
  //      1. They might not contain the faces of the simplices as component subspaces
  //      2. The component spaces contain no gluing information - i.e., we don't have any connection between a simplex and its faces,
  //         or a simplex and the simplices of which it is a face.
  // To solve this problem, we initialize our complex just by copying in the component spaces,
  // but we'll build up *all* component simplices and store them.
  // Note: The simplices of maximal dimension are exactly those that belong to componentSpaces. The only ways a simplex can become part of our complex
  // is if it is passed in as a component space or is a face of a component space. The simplices of maximal dimension cannot be proper faces of any other simplex
  // as this would contradict maximality. Therefore, copying in component spaces yields all simplices of maximal dimension. This argument cannot be applied to
  // simplices of lower dimension as these are perhaps proper faces of component spaces that are not themselves component spaces. For that reason, we need to
  // come up with an update process to incorporate them.
  private val initSimplexMap: Map[Int, Vector[Simplex]] = componentSpaces.groupBy(_.vertices.length - 1).map({case (k, v) => (k, v.map(new Simplex(_)))}).withDefaultValue(Vector())

  // Assume: We've already correctly found all n-simplices with n > nextSimplexDimension. This update step should correctly yield the (nextSimplexDimension)-simplices,
  // I.e., after this update step, our map should correctly contain all n-simplices with n >= nextSimplexDimension.
  // To accomplish this, we find all simplices of higher dimension, look at their (nextSimplexDimension)-simplex faces, and incorporate those of which
  // that are actually new. Since simplicial face maps form a directed system, we don't need to worry about faces of faces.
  // ************** Important: Since we're going to use this in a fold, we can actually do this a bit backwards...
  // **************            Instead of looking at *all* higher simplices, we can just look at simplices of dimension *ONE* higher.
  private val getUpdatedSimplexMap: (Int, Map[Int, Vector[Simplex]]) => Map[Int, Vector[Simplex]] = { case (nextSimplexDimension, currentSimplexMap) =>

    // Get all simplices currently stored in the map that have dimension (nextSimplexDimension+1). Our inductive hypothesis is that
    // these are actually all simplices of this dimension.
    val simplicesOfDimPlusOne: Iterable[Simplex] = currentSimplexMap.filter(_._1 == nextSimplexDimension+1).flatMap(_._2)

    // Get all subsets of vertices of simplices of dimension (nextSimplexDimension+1). Even though we are storing these subsets as vectors,
    // we don't want to be affected by order, so take this collection distinctBy a set representation.
    // Also, two simplices might intersect at a face, so we don't want to double (or many-times) add this intersection.
    val faceVerticesFromHigherSimplices: Vector[Set[Point]] = simplicesOfDimPlusOne.flatMap((simplex: Simplex) => {
      simplex.points.toVector.combinations(nextSimplexDimension+1).map(_.toSet)
    }).toVector.distinctBy(_.toSet)

    // We only want to build a new simplex from a vector of points that is *actually* new. I.e., some of these sets we find
    // might already be represented in the simplicial complex. Filter out the collections of points whose set representation is already present.
    val newVertices: Vector[Set[Point]] = faceVerticesFromHigherSimplices.filter((candidatePoints: Set[Point]) => {
      val currentSimplicesOfThisDimension: Vector[Simplex] = currentSimplexMap(nextSimplexDimension)

      // We don't want to include this set of candidates if there already exists a simplex of this dimension with the candidate set as its vertex set.
      !currentSimplicesOfThisDimension.exists((simplexOfThisDim: Simplex) => simplexOfThisDim.points == candidatePoints)
    })

    // Now that we've identified the vertex sets that are truly new, we'll build new simplices out of them.
    val newSimplices: Vector[Simplex] = newVertices.map(Simplex)

    // The updated map is now just the previous map with the new simplices appended in the right place...
    currentSimplexMap.updated(nextSimplexDimension, currentSimplexMap(nextSimplexDimension) ++ newSimplices)
  }

  // Since we know the simplices of dimension maxComponentDimension are correct, fall through the possible dimensions while collecting
  // faces along the way.
  /** The simplices of the complex organized by dimension.
   *
   *  This map only stores the simplices of the complex. It does not store how those simplices are connected.
   *  For this information, see [[faceMap]].
   */
  val nSimplices: Map[Int, Vector[Simplex]] = forcedNSimplices match {
    case Some(forced) => forced
    case None => (0 until maxComponentDimension).foldRight(initSimplexMap)(getUpdatedSimplexMap)
  }

  /** The simplices of the complex. */
  val simplices: Vector[Simplex] = nSimplices.values.flatten.toVector

  println("Computing n face map")
  /** Maps a number n and a simplex s to the collection of n-faces of s. */
  val nFaceMap: Map[(Int, Simplex), Vector[Simplex]] = forcedNFaceMap match {
    case Some(forced) => forced
    case None =>
      {
        for (
          simplex <- simplices;
          n <- 0 to maxComponentDimension
        ) yield  {(
            (n, simplex), // Key
            simplex // Start with the simplex in the key
              .points // Look at its points
              .subsets(n+1) // Take n+1 at a time (an n-simplex has n+1 vertices)
              .flatMap(subset => simplices.find(_.points == subset)) // Try to find the simplex that has the given subset as its vertex set
              .toVector // Collect all these into a vector
        )}
      }.toMap
  }

  println("Computing face map")
  /** Maps a simplex to all of its faces. */
  val faceMap: Map[Simplex, Vector[Simplex]] = simplices.map((simplex: Simplex) => {

    (simplex, (0 to maxComponentDimension).map(n => nFaceMap(n, simplex)).reduce(_ ++ _))

  }).toMap
  println("Computing ancestor map")

  /** Maps a simplex to the collection of simplices that contain it. */
  val ancestorMap: Map[Simplex, Vector[Simplex]] = {

    val mutableAncestorMap: scala.collection.mutable.Map[Simplex, Vector[Simplex]] = scala.collection.mutable.Map() ++ simplices.map(simplex => (simplex, Vector[Simplex]())).toMap

    simplices.foreach(simplex => {
      val descendents: Vector[Simplex] = faceMap(simplex)
      descendents.foreach(descendent => mutableAncestorMap.update(descendent, mutableAncestorMap(descendent) :+ simplex))
    })


//    (simplex, simplices.filter((candidateParent: Simplex) => simplex.points.subsetOf(candidateParent.points)))
    Map() ++ mutableAncestorMap
  }

  def printStatistics(numBins: Int = 20): Unit = {

    val edgeLengths: Vector[Double] = nSimplices(1).flatMap(simplex => {
      val pts = simplex.points.toVector
      pts(0).dist(pts(1))
    })

    def makeHistogram(values: Vector[Double]): Vector[((Double, Double), Int)] = {

      val minValue = values.min
      val maxValue = values.max

      val binSize = (maxValue - minValue) / numBins

      def getBinCoordinates(v: Double): (Double, Double) = {
        val idx: Double = ((v-minValue) / binSize).floor

        (minValue + binSize * idx, minValue + binSize * (idx + 1.0))
      }

      values.groupBy(getBinCoordinates).toVector.map(p => (p._1, p._2.length)).sortBy(_._1._1)

    }


    val edgeHist: Vector[((Double, Double), Int)] = makeHistogram(edgeLengths)
    val heaviestBinWeight: Int = edgeHist.maxBy(_._2)._2

    val minEdgeLength: Double = edgeLengths.min
    val maxEdgeLength: Double = edgeLengths.max
    val meanEdgeLength: Double = edgeLengths.sum / edgeLengths.length.toDouble
    val medianEdgeLength: Double = edgeLengths(edgeLengths.length / 2)

    val sseEdgeLength: Double = edgeLengths.map(length => (length - meanEdgeLength) * (length - meanEdgeLength)).sum
    val standardDevEdgeLength: Double = scala.math.sqrt(sseEdgeLength / edgeLengths.length.toDouble)

    println("")
    println("Minimum edge length: " + minEdgeLength)
    println("Mean edge length: " + meanEdgeLength)
    println("Standard dev edge length: " + standardDevEdgeLength)
    println("Median edge length: " + medianEdgeLength)
    println("Maximum edge length: " + maxEdgeLength)
    println("")
    println("Histogram of edge lengths:")
    edgeHist foreach println
    println("")
    println("Histogram as line graph normalized to heaviest bin:")
    edgeHist.foreach(datum => println((datum._1._1 + datum._1._2) / 2.0 + ", " + (datum._2.toDouble / heaviestBinWeight.toDouble)))
    println("")
  }

  def getComplexWithout(simplicesToExclude: Vector[Simplex]): GeometricSimplicialComplex = {

    // Problem: To *TRULY* exclude a simplex, we must exclude all of its ancestors.
    // Note: The ancestor map is an order relation. In particular, it is reflexive, so we don't accidentally remove the
    // original simplices here.
    val allSimplicesToExclude: Vector[Simplex] = simplicesToExclude.flatMap(ancestorMap)

    val newNSimplices: Map[Int, Vector[Simplex]] = nSimplices.map({case (n, nSimplices) =>
      (n, nSimplices.diff(allSimplicesToExclude))
    }).filter(_._2.nonEmpty)

    new GeometricSimplicialComplex(Vector(), Some(newNSimplices))
  }


  def exportToFile(filename: String): Unit = {
    import java.io._

    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))

    val simplicesWithIndex: Vector[(Simplex, Int)] = simplices.zipWithIndex

    def brittleSimplexToString(simplex: Simplex): String = {
      simplex.points.toVector.map(point => point.coordinates.map(_.toString).reduce(_ + "," + _)).reduce(_ + "|" + _)
    }

    simplicesWithIndex.foreach(pair => bw.write(brittleSimplexToString(pair._1) + ":" + pair._2.toString + "\n"))

    val simplexMap: Map[Simplex, Int] = simplicesWithIndex.toMap

    bw.write("Face Map\n")

//    faceMap.foreach(pair => bw.write(simplexMap(pair._1) + "->" + pair._2.map(simplexMap).map(_.toString).reduce(_ + "," + _) + "\n"))

    bw.write(faceMap.map(pair => simplexMap(pair._1) + "->" + pair._2.map(simplexMap).map(_.toString).reduce(_ + "," + _)).reduce(_ + "\n" + _))

    bw.close()
  }



  def bruteForceProject(point: Point): Point = {

    simplices
      .zipWithIndex
      .flatMap({case (simplex, k) =>
//        println("Brute force working on simplex " + k + " of " + numBruteForceSimplices)
        simplex.getProjection(point)
      })
      .minBy(proj => point.dist(proj))

  }

}

object GeometricSimplicialComplex {

  def loadFromFile(filename: String,
                   faceMapLine: String = "Face Map",
                   faceMapPrimarySep: String = "->",
                   faceMapIndexSep: String = ",",
                   pointSep: String = "\\|",
                   pointInternalSep: String = ",",
                   simplexIndexSep: String = ":"): Option[GeometricSimplicialComplex] = {

    val bufferedComplexSource: BufferedSource = Source.fromFile(filename)
    val lines = bufferedComplexSource.getLines().toVector

    val faceIndex: Option[Int] = lines.zipWithIndex.find(_._1 == faceMapLine).map(_._2)

    faceIndex match {
      case Some(index) =>
        val (simplexLines, faceMapLines) = {
//          println("Face index:")
//          println(faceIndex)
//          println(index)
          val split = lines.splitAt(index)

          val split1Vector = split._1
          val split2Vector = split._2

//          println("Split 1 first")
//          println(split1Vector.head)
//          println("Split 1 last")
//          println(split1Vector.last)
//          println("Split 2 first and second")
//          split2Vector.take(2) foreach println

          (split1Vector, split2Vector.tail)
        }

        val simplexIndexMap: Map[Int, Simplex] = simplexLines.map((simplexString: String) => {

          val (pointsString, indexString) = {
            val split = simplexString.split(simplexIndexSep)
//            println("Original:")
//            println(simplexString)
//            println("Split:")
//            split foreach println
            (split(0), split(1))
          }

          val index: Int = indexString.toInt

          val pointStrings = pointsString.split(pointSep)

//          println("Points string")
//          println(pointsString)
//          println("Sep")
//          println(pointSep)
//          println("Point strings")
//          pointStrings foreach println

          val points: Set[Point] = pointStrings.map(str => {
//            println("Point string")
//            println(str)
            Point(str.split(pointInternalSep).map(_.toDouble).toVector)
          }).toSet

          val simplex: Simplex = Simplex(points)

          (index, simplex)
        }).toMap

        val simplices = simplexIndexMap.values

        val nSimplexMap: Map[Int, Vector[Simplex]] = simplices.groupBy(_.points.size).map(p => (p._1 - 1, p._2.toVector))

        val faceMap: Map[Simplex, Vector[Simplex]] = faceMapLines.map(str => {
          val (keyString, valueString) = {
            val split = str.split(faceMapPrimarySep)
            (split(0), split(1))
          }

          val key: Simplex = simplexIndexMap(keyString.toInt)
          val values: Vector[Simplex] = valueString.split(faceMapIndexSep).map(indexStr => simplexIndexMap(indexStr.toInt)).toVector

          (key, values)
        }).toMap

        val maxSimplexDimension: Int = simplices.map(_.points.size).max - 1

//        println("Max simplex dimension")
//        println(maxSimplexDimension)
//        assert(false)

        val nFaceMap: Map[(Int, Simplex), Vector[Simplex]] = {for (
          n <- 0 to maxSimplexDimension;
          simplex <- simplices
        ) yield (
          (n, simplex),
          faceMap(simplex).filter(_.points.size == n+1)
        )}.toMap


        bufferedComplexSource.close()

        Some(new GeometricSimplicialComplex(Vector(), Some(nSimplexMap), Some(nFaceMap)))
      case None =>
        bufferedComplexSource.close()
        None
    }
  }

}



object LoadTest extends App {

  println("Loading...")

  val loadedGeomComp: Option[GeometricSimplicialComplex] = GeometricSimplicialComplex.loadFromFile("Datasets/UnculledNormalizedIrisComplex.txt")

  println("Loaded!")

  println(loadedGeomComp)


  loadedGeomComp match {
    case Some(complex) =>
      complex.printStatistics(30)
    case None =>
      println("Red alert! Failed to load simplicial complex from file!")
  }



  val culledGeomComp: Option[GeometricSimplicialComplex] = GeometricSimplicialComplex.loadFromFile("Datasets/CulledNormalizedIrisComplex.txt")

  culledGeomComp match {
    case Some(complex) =>
      complex.printStatistics(8)
      println(complex.bruteForceProject(Point(Vector(0.0, 0.0, 0.0, 0.0))))
    case None =>
      println("Red alert! Failed to load simplicial complex from file!")
  }

}



object GeometricSimplicialComplexTest extends App {

  val points: Vector[Point] = Vector(
    Point(Vector(0.0, 0.0, 0.0)),
    Point(Vector(1.0, 0.0, 0.0)),
    Point(Vector(0.0, 1.0, 0.0)),
    Point(Vector(0.0, 0.0, 1.0)),
    Point(Vector(1.0, 1.0, 1.0))
  )

  val pointNames: Point => String = {
    case Point(Vector(0.0, 0.0, 0.0)) => "O"
    case Point(Vector(1.0, 0.0, 0.0)) => "X"
    case Point(Vector(0.0, 1.0, 0.0)) => "Y"
    case Point(Vector(0.0, 0.0, 1.0)) => "Z"
    case Point(Vector(1.0, 1.0, 1.0)) => "S"
    case _ => "Unknown Point"
  }

  val delaunay: Vector[PointedAffineSpace] = Delaunay.DelaunayUtil.getDelaunaySimplicialization(points)

  delaunay.foreach((space: PointedAffineSpace) => {
    println("(" + space.vertices.map(pointNames).reduce(_ + ", " + _) + ")")
  })

  val simplicialization: GeometricSimplicialComplex = new GeometricSimplicialComplex(delaunay)

  println(simplicialization)

  simplicialization.nSimplices foreach { case (n, nSimplices) =>
    println(n + "-Simplices:")
    nSimplices.foreach((simplex: Simplex) =>
      println(if (simplex.points.nonEmpty) {"[" + simplex.points.map(pointNames).reduce(_ + ", " + _) + "]"} else "[]")
    )
  }

}


object GeometricSimplicialComplexTest2 extends App {

  val points: Vector[Point] = Vector(
    Point(Vector(0.0, 0.0)),
    Point(Vector(3.0, 4.0)),
    Point(Vector(4.0, 3.0)),
    Point(Vector(0.0, 25.0/7.0)),
  )

  val k = 25.0 / 7.0

  val pointNames: Point => String = {
    case Point(Vector(0.0, 0.0)) => "O"
    case Point(Vector(3.0, 4.0)) => "Y"
    case Point(Vector(4.0, 3.0)) => "X"
    case Point(Vector(0.0, k)) => "S"
    case _ => "Unknown Point"
  }

  val delaunay: Vector[PointedAffineSpace] = Delaunay.DelaunayUtil.getDelaunaySimplicialization(points)

  delaunay.foreach((space: PointedAffineSpace) => {
    println("(" + space.vertices.map(pointNames).reduce(_ + ", " + _) + ")")
  })

  val simplicialization: GeometricSimplicialComplex = new GeometricSimplicialComplex(delaunay)

  println(simplicialization)

  simplicialization.nSimplices foreach { case (n, nSimplices) =>
    println(n + "-Simplices:")
    nSimplices.foreach((simplex: Simplex) =>
      println(if (simplex.points.nonEmpty) {"[" + simplex.points.map(pointNames).reduce(_ + ", " + _) + "]"} else "[]")
    )
  }

}
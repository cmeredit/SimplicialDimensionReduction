package DimensionReduction

import DimensionReduction.Delaunay.PointedAffineSpace

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
    (simplex, simplices.filter((candidateFace: Simplex) => candidateFace.points.subsetOf(simplex.points)))
  }).toMap

  /** Maps a number n and a simplex s to the collection of n-faces of s. */
  val nFaceMap: Map[(Int, Simplex), Vector[Simplex]] = simplices.zip(0 to maxComponentDimension).map({case (simplex, n) =>
    ((n, simplex), simplices.filter((candidateFace: Simplex) => candidateFace.points.subsetOf(simplex.points) && candidateFace.points.size == n))
  }).toMap

  /** Maps a simplex to the collection of simplices that contain it. */
  val ancestorMap: Map[Simplex, Vector[Simplex]] = simplices.map((simplex: Simplex) => {
    (simplex, simplices.filter((candidateParent: Simplex) => simplex.points.subsetOf(candidateParent.points)))
  }).toMap

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

}
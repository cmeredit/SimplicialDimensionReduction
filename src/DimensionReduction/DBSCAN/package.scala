package DimensionReduction

/** Provides objects and traits for implementing the DBSCAN algorithm.
 *
 *  ==Overview==
 *  The main object to use is [[DimensionReduction.DBSCAN.DBSCANUtil]], e.g.,
 *  {{{
 *    val epsilon: Double = 0.1
 *    val minPts: Int = 3
 *    val points: Vector[Point] = Vector(Point(Vector(0.0, 0.0, 0.0)), Point(Vector(0.0, 0.0, 1.0)))
 *    val clusteredPoints: Vector[DBPoint] = DBSCANUtil.cluster(epsilon, minPts)(points)
 *  }}}
 *
 *  If you intend to cluster multiple sets of points according to the same parameters, you can fill
 *  just the first parameter list of DBSCANUtil.cluster:
 *  {{{
 *    val epsilon: Double = ???
 *    val minPts: Int = ???
 *    val clusterer: Vector[Point] => Vector[DBPoint] = DBSCANUtil.cluster(epsilon, minPts)
 *
 *    val firstCollectionOfPoints: Vector[Point] = ???
 *    val secondCollectionOfPoints: Vector[Point] = ???
 *
 *    val firstClusteredPoints: Vector[DBPoint] = clusterer(firstCollectionOfPoints)
 *    val secondClusteredPoints: Vector[DBPoint] = clusterer(secondCollectionOfPoints)
 *  }}}
 *
 */
package object DBSCAN

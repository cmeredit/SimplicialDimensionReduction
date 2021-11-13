package DimensionReduction.DBSCAN

import DimensionReduction.Point

/** Utility object for accessing the DBSCAN algorithm. */
object DBSCANUtil {

  /** Clusters points by the DBSCAN algorithm.
   *
   *  @see https://en.wikipedia.org/wiki/DBSCAN
   *  @param epsilon The distance used to determine if two points are adjacent.
   *  @param minPoints The minimum number of neighbors for a point to be considered a "core" point.
   *  @param points The points to be clustered.
   *  @return The clustered points.
   */
  def cluster(epsilon: Double, minPoints: Int)(points: Vector[Point]): Vector[DBPoint] = {
    // TODO: Remove repeated neighborhood computation - cache this somewhere

    // Avoid taking square roots
    val epsilonSquared: Double = epsilon * epsilon

    // Determine what points are definitely core points
    val pointsWithValidCore: Vector[DBPoint] = points.map(p => {
      val numNeighbors: Int = points.filter(_ != p).count(_.distSquared(p) match {
        case Some(d) => d <= epsilonSquared
        case None => false
      })

      val clusterType: Option[ClusterType] = if (numNeighbors >= minPoints) Some(Core) else None

      new DBPoint(p.coordinates, Vector(), clusterType)
    })

    // Determine what points are boundary or outlier points
    val pointsWithValidClusters: Vector[DBPoint] = pointsWithValidCore.map(p => {
      val clusterType: Option[ClusterType] = p.clusterType match {
        case Some(value) => Some(value)
        case None =>
          val hasCoreNeighbor: Boolean = pointsWithValidCore.filter(_ != p).filter(_.distSquared(p) match {
            case Some(d) => d <= epsilonSquared
            case None => false
          }).exists(_.clusterType.contains(Core))

          if (hasCoreNeighbor) Some(Boundary) else Some(Outlier)
      }

      new DBPoint(p.coordinates, Vector(), clusterType)
    })

    // Actually compute the neighbors now that types are determined
    val dbPoints: Vector[DBPoint] = pointsWithValidClusters.map(p => {
      val neighbors: Vector[DBPoint] = pointsWithValidClusters.filter(_ != p).filter(_.distSquared(p) match {
        case Some(d) => d <= epsilonSquared
        case None => false
      })

      new DBPoint(p.coordinates, neighbors, p.clusterType)
    })

    dbPoints
  }

}

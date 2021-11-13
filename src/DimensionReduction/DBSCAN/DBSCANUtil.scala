package DimensionReduction.DBSCAN

import DimensionReduction.Point

sealed trait ClusterType
case object Core extends ClusterType
case object Boundary extends ClusterType
case object Outlier extends ClusterType

// ClusterType Option: Some == Determined, None == Undetermined
class DBPoint(c: List[Double], val neighbors: List[DBPoint], val clusterType: Option[ClusterType]) extends Point(c) {
  override def toString: String =
    "DBPoint(" +
      "Coordinates: " + c.toString() + ", " +
      "Dimension: " + dimension.toString + ", " +
      "Neighbors: " + neighbors.map(_.coordinates.toString()) + ", " +
      "Cluster Type: " + {
      clusterType match {
        case Some(value) => value match {
          case Core => "Core"
          case Boundary => "Boundary"
          case Outlier => "Outlier"
        }
        case None => "Undetermined"
      }
    } +
    ")"
}

object DBSCANUtil {

  def cluster(epsilon: Double, minPoints: Int)(points: List[Point]): List[DBPoint] = {
    // TODO: Remove stupid repeated neighborhood computation - cache this somewhere

    // Avoid taking square roots
    val epsilonSquared: Double = epsilon * epsilon

    val pointsWithValidCore: List[DBPoint] = points.map(p => {
      val numNeighbors: Int = points.filter(_ != p).count(_.distSquared(p) match {
        case Some(d) => d <= epsilonSquared
        case None => false
      })

      val clusterType: Option[ClusterType] = if (numNeighbors >= minPoints) Some(Core) else None

      new DBPoint(p.coordinates, List(), clusterType)
    })

    val pointsWithValidClusters: List[DBPoint] = pointsWithValidCore.map(p => {
      val clusterType: Option[ClusterType] = p.clusterType match {
        case Some(value) => Some(value)
        case None =>
          val hasCoreNeighbor: Boolean = pointsWithValidCore.filter(_ != p).filter(_.distSquared(p) match {
            case Some(d) => d <= epsilonSquared
            case None => false
          }).exists(_.clusterType.contains(Core))

          if (hasCoreNeighbor) Some(Boundary) else Some(Outlier)
      }

      new DBPoint(p.coordinates, List(), clusterType)
    })

    val dbPoints: List[DBPoint] = pointsWithValidClusters.map(p => {
      val neighbors: List[DBPoint] = pointsWithValidClusters.filter(_ != p).filter(_.distSquared(p) match {
        case Some(d) => d <= epsilonSquared
        case None => false
      })

      new DBPoint(p.coordinates, neighbors, p.clusterType)
    })

    dbPoints
  }

}

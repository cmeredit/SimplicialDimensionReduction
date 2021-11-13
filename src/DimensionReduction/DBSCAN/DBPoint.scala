package DimensionReduction.DBSCAN

import DimensionReduction.Point

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
package DimensionReduction.DBSCAN

import DimensionReduction.Point

// ClusterType Option: Some == Determined, None == Undetermined

/** Represents a point that has been clustered by the DBSCAN algorithm.
 *
 *  @param c The coordinates of the point.
 *  @param neighbors Nearby points given by DBSCAN.
 *  @param clusterType Result of DBSCAN clustering. If clusterType is nonempty, then the type of this point has been determined. Otherwise, it is undetermined.
 */
class DBPoint(c: Vector[Double], val neighbors: Vector[DBPoint], val clusterType: Option[ClusterType]) extends Point(c) {
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
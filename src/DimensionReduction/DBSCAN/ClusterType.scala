package DimensionReduction.DBSCAN

/** Trait representing how a point is clustered by DBSCAN */
sealed trait ClusterType

/** Represents that this is a core point under the DBSCAN algorithm (num epsilon neighbors >= min neighbors). */
case object Core extends ClusterType

/** Represents that this is a boundary point under the DBSCAN algorithm (exists a core epsilon neighbor). */
case object Boundary extends ClusterType

/** Represents that this is an outlier point under the DBSCAN algorithm (no core neighbors and num epsilon neighbors < min neighbors). */
case object Outlier extends ClusterType

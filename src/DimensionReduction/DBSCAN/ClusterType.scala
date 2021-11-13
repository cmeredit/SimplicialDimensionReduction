package DimensionReduction.DBSCAN

sealed trait ClusterType
case object Core extends ClusterType
case object Boundary extends ClusterType
case object Outlier extends ClusterType

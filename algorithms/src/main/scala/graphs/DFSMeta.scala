package graphs

case class DFSMeta[V](
    colors: Map[V, Color],
    parents: Map[V, Option[V]],
    discoveryTime: Map[V, Int],
    finishedTime: Map[V, Int]
)

case class BFSMeta[V](
    colors: Map[V, Color],
    distances: Map[V, Int],
    parents: Map[V, Option[V]]
)

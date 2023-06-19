package graphs

case class DFSMeta[V](components: Map[V, Int])

case class BFSMeta[V](
    colors: Map[V, Color],
    distances: Map[V, Int],
    parents: Map[V, Option[V]]
)

package graphs

case class DFSMeta[V](
    componentsCount: Int,
    components: Map[V, Int],
    tin: Map[V, Int],
    tout: Map[V, Int]
)

case class BFSMeta[V](
    colors: Map[V, Color],
    distances: Map[V, Int],
    parents: Map[V, Option[V]]
)

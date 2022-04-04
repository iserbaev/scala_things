package graphs

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

/** Adjacency list representation (список смежности)
  * Для каждой вершины u Adj[u] состоит из всех вершин смежных с u в графе G
  */
case class Adj2[V](g: Map[V, List[V]]) {
  import Adj2._

  def adjacent(u: V, v: V): Boolean =
    g(u).contains(v)

  def values: List[V] = g.keys.toList

  def dfs: DFSMeta[V] = {
    val component = mutable.Map.empty[V, Int]
    val tin       = mutable.Map.empty[V, Int]
    val tout      = mutable.Map.empty[V, Int]

    val counter           = new AtomicInteger(0)
    val componentsCounter = new AtomicInteger(0)

    def dfs_(v: V, num: Int): Unit = {
      tin.update(v, counter.incrementAndGet())
      component.update(v, num)

      g(v).foreach(u => if (!component.contains(u)) dfs_(u, num))

      tout.update(v, counter.incrementAndGet())
    }

    g.keys.foreach { v =>
      if (!component.contains(v)) dfs_(v, componentsCounter.incrementAndGet())
    }

    DFSMeta(componentsCounter.get(), component.toMap, tin.toMap, tout.toMap)
  }

}

object Adj2 {
  case class DFSMeta[V](
    componentsCount: Int,
    components:      Map[V, Int],
    tin:             Map[V, Int],
    tout:            Map[V, Int]
  )

  def apply[V](edgesMappings: Map[String, List[V]]): Adj2[V] = {

    val gMap = edgesMappings.values.foldLeft(mutable.Map.empty[V, List[V]]) {
      case (gi, pair) =>
        if (pair.nonEmpty) {
          require(pair.length == 2, s"pair.length != 2, fact =  ${pair.length}")
          val (v1, v2) = pair.head -> pair(1)
          gi.update(v1, v2 :: gi.getOrElse(v1, List.empty))
          gi.update(v2, v1 :: gi.getOrElse(v2, List.empty))
        }

        gi
    }

    new Adj2(gMap.toMap)
  }
}

package graphs

import graphs.Color.{ Black, Grey, White }

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object GraphsProcessor {

  def dfs[V](holder: AdjacentHolder[V]): DFSMeta[V] = {
    val component = mutable.Map.empty[V, Int]
    val tin       = mutable.Map.empty[V, Int]
    val tout      = mutable.Map.empty[V, Int]

    val counter           = new AtomicInteger(0)
    val componentsCounter = new AtomicInteger(0)

    def dfs_(v: V, num: Int): Unit = {
      tin.update(v, counter.incrementAndGet())
      component.update(v, num)

      holder.adjacentVertices(v).foreach(u => if (!component.contains(u)) dfs_(u, num))

      tout.update(v, counter.incrementAndGet())
    }

    holder.vertices.foreach { v =>
      if (!component.contains(v)) dfs_(v, componentsCounter.incrementAndGet())
    }

    DFSMeta(componentsCounter.get(), component.toMap, tin.toMap, tout.toMap)
  }

  def bfs[V](s: V, holder: AdjacentHolder[V]): BFSMeta[V] = {
    val colors    = mutable.Map.empty[V, Color]
    val distances = mutable.Map.empty[V, Int]
    val parents   = mutable.Map.empty[V, Option[V]]

    holder.vertices.foreach { v =>
      colors.update(v, White)
      distances.update(v, -1)
      parents.update(v, None)
    }

    colors.update(s, Grey)
    distances.update(s, 0)

    val queue = mutable.Queue.empty[V]
    queue.enqueue(s)

    while (queue.nonEmpty) {
      val u = queue.dequeue()

      holder.vertices
        .foreach { v =>
          if (colors(v) == White && holder.adjacent(u, v)) {
            colors.update(v, Grey)
            distances.update(v, distances(u) + 1)
            parents.update(v, Some(u))
            queue.enqueue(v)
          }
        }

      colors.update(u, Black)
    }

    BFSMeta(colors.toMap, distances.toMap, parents.toMap)
  }

}

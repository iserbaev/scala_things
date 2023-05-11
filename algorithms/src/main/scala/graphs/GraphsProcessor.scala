package graphs

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object GraphsProcessor {

  def dfs(holder: AdjacentHolder): DFSMeta[Int] = {
    val component = mutable.Map.empty[Int, Int]
    val tin       = mutable.Map.empty[Int, Int]
    val tout      = mutable.Map.empty[Int, Int]

    val counter           = new AtomicInteger(0)
    val componentsCounter = new AtomicInteger(0)

    def dfs_(v: Int, num: Int): Unit = {
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

  def bfs(s: Int, holder: AdjacentHolder): BFSMeta[Int] = {
    val colors    = mutable.Map.empty[Int, Color]
    val distances = mutable.Map.empty[Int, Int]
    val parents   = mutable.Map.empty[Int, Option[Int]]

    holder.vertices.foreach { v =>
      colors.update(v, Color.White)
      distances.update(v, -1)
      parents.update(v, None)
    }

    colors.update(s, Color.Grey)
    distances.update(s, 0)

    val queue = mutable.Queue.empty[Int]
    queue.enqueue(s)

    while (queue.nonEmpty) {
      val u = queue.dequeue()

      holder.vertices
        .foreach { v =>
          if (colors(v) == Color.White && holder.adjacent(u, v)) {
            colors.update(v, Color.Grey)
            distances.update(v, distances(u) + 1)
            parents.update(v, Some(u))
            queue.enqueue(v)
          }
        }

      colors.update(u, Color.Black)
    }

    BFSMeta(colors.toMap, distances.toMap, parents.toMap)
  }

}

package graphs

import scala.collection.mutable

object GraphsProcessor {

  def dfs(holder: AdjacentHolder): DFSMeta[Int] = {
    val components = mutable.Map.empty[Int, Int]
    val tin       = mutable.Map.empty[Int, Int]
    val tout      = mutable.Map.empty[Int, Int]

    var counter           = 0
    var componentsCounter = 0

    def dfs_(v: Int, component: Int): Unit = {
      counter += 1
      tin.update(v, counter)
      components.update(v, component)

      holder.adjacentVertices(v).foreach(u => if (!components.contains(u)) dfs_(u, component))

      counter += 1
      tout.update(v, counter)
    }

    holder.vertices.foreach { v =>
      if (!components.contains(v)) {
        componentsCounter += 1
        dfs_(v, componentsCounter)
      }
    }

    componentsCounter += 1
    DFSMeta(componentsCounter, components.toMap, tin.toMap, tout.toMap)
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

      holder.adjacentVertices(u)
        .foreach { v =>
          if (colors(v) == Color.White) {
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

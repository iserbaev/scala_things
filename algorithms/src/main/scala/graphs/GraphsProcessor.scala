package graphs

import scala.collection.mutable

object GraphsProcessor {

  def components(holder: AdjacentHolder): Map[Int, Int] = {
    val components        = scala.collection.mutable.Map.empty[Int, Int]
    var componentsCounter = 0

    @scala.annotation.tailrec
    def dfs_(adjacents: Set[Int], component: Int): Unit =
      if (adjacents.nonEmpty) {
        val v = adjacents.head
        if (!components.contains(v)) {
          components.update(v, component)
          dfs_(adjacents.tail ++ holder.adjacentVertices(v), component)
        } else {
          dfs_(adjacents.tail, component)
        }
      } else {
        ()
      }

    holder.vertices.foreach { v =>
      if (!components.contains(v)) {
        componentsCounter += 1
        components.update(v, componentsCounter)
        dfs_(holder.adjacentVertices(v), componentsCounter)
      }
    }

    components.toMap
  }

  def dfs(holder: AdjacentHolder): DFSMeta[Int] = {
    val colors        = mutable.Map.empty[Int, Color]
    val parents       = mutable.Map.empty[Int, Option[Int]]
    val discoveryTime = mutable.Map.empty[Int, Int]
    val finishedTime  = mutable.Map.empty[Int, Int]

    holder.vertices.foreach { v =>
      colors.update(v, Color.White)
      parents.update(v, None)
    }

    var time = 0
    def dfsVisit(u: Int): Unit = {
      time += 1
      discoveryTime.update(u, time)
      colors.update(u, Color.Grey)

      holder.adjacentVertices(u).foreach { uv =>
        if (colors(uv) == Color.White) {
          parents.update(uv, Some(u))
          dfsVisit(uv)
        }
      }

      colors.update(u, Color.Black)
      time += 1
      finishedTime.update(u, time)
    }

    holder.vertices.foreach { v =>
      if (colors(v) == Color.White) {
        dfsVisit(v)
      }
    }

    DFSMeta(colors.toMap, parents.toMap, discoveryTime.toMap, finishedTime.toMap)
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

      holder
        .adjacentVertices(u)
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

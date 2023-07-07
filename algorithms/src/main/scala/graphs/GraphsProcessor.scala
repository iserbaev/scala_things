package graphs

import scala.collection.mutable

object GraphsProcessor {

  sealed trait Color

  object Color {
    case object White extends Color

    case object Grey extends Color

    case object Black extends Color
  }

  case class DFSMeta[V](
      colors: Map[V, Color],
      parents: Map[V, Option[V]],
      discoveryTime: Map[V, Int],
      finishedTime: Map[V, Int],
      cycles: Map[V, V]
  )

  case class BFSMeta[V](
      colors: Map[V, Color],
      distances: Map[V, Int],
      predecessor: Map[V, Option[V]]
  )

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

  // O(V + E)
  def dfs(holder: AdjacentHolder): DFSMeta[Int] = {
    val colors        = mutable.Map.empty[Int, Color]
    val parents       = mutable.Map.empty[Int, Option[Int]]
    val discoveryTime = mutable.Map.empty[Int, Int]
    val finishedTime  = mutable.Map.empty[Int, Int]
    val cycles        = mutable.Map.empty[Int, Int]

//    val colors = Array.fill[Color](holder.vertices.length)(Color.White)
//    val parents = Array.fill(holder.vertices.length)(Option.empty[Int])
//    val discoveryTime = Array.fill(holder.vertices.length)(-1)
//    val finishedTime = Array.fill(holder.vertices.length)(-1)

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
        } else if (colors(uv) == Color.Grey) {
          cycles.update(uv, u)
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

    DFSMeta(colors.toMap, parents.toMap, discoveryTime.toMap, finishedTime.toMap, cycles.toMap)
  }

  // O(V + E)
  def topologicalSort(holder: AdjacentHolder): IndexedSeq[Int] =
    dfs(holder).finishedTime.toIndexedSeq
      .sortBy(_._2)(Ordering.Int.reverse) // sort by finished time in descending order
      .map(_._1)                          // return sorted vertices

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

  def shortestPathLee(start: Int, end: Int, holder: AdjacentHolder): Int = {
    val length = holder.vertices.length
    require(start < length && end < length)
    val distances = Array.fill(length)(-1)
    val layers    = Array.fill(length + 1)(Array.empty[Int])

    layers.update(0, Array(start))
    distances.update(start, 0)

    var currentDistance = 0

    while (layers(currentDistance).nonEmpty) {
      layers(currentDistance)
        .foreach { currentVertex =>
          holder.adjacentVertices(currentVertex).foreach { nextVertex =>
            if (distances(nextVertex) == -1) {
              layers.update(currentDistance + 1, layers(currentDistance + 1).appended(nextVertex))
              distances.update(nextVertex, currentDistance + 1)
            }
          }
        }
      currentDistance += 1
    }

    distances(end)
  }

  def shortestPathBellmanFord(
      source: Int,
      holder: AdjacentHolder,
      weight: (Int, Int) => Int
  ): (Array[Int], Array[Option[Int]]) = {
    val (distances, predecessors) = initializeSingleSource(source, holder)

    holder.vertices.tail.foreach { _ =>
      holder.edges.foreach { case (u, v) => relax(u, v, weight, distances, predecessors) }
    }

    holder.edges.foreach { case (u, v) =>
      if (distances(v) > distances(u) + weight(u, v)) println("Graph contains a negative-weight cycle")
    }

    (distances, predecessors)
  }

  // O(V + E)
  def dagShortestPath(holder: AdjacentHolder, weight: (Int, Int) => Int, source: Int) = {
    val sortedVertices            = topologicalSort(holder)
    val (distances, predecessors) = initializeSingleSource(source, holder)
    sortedVertices.foreach { u =>
      holder.adjacentVertices(u).foreach { v =>
        relax(u, v, weight, distances, predecessors)
      }
    }

    (distances, predecessors)
  }

  // Weighted oriented graph,
  // where for any (u,v) in E, weight(u,v) >= 0
  def dijkstra(holder: AdjacentHolder, weight: (Int, Int) => Int, source: Int) = {
    val (distances, predecessors) = initializeSingleSource(source, holder)
    val S                         = Set.newBuilder[Int]
    val customOrdering            = Ordering.by[Int, Int](i => distances(i)).reverse
    val priorityQueue             = mutable.PriorityQueue.from(holder.vertices)(customOrdering) // ASC heap

    while (priorityQueue.nonEmpty) {
      val u = priorityQueue.dequeue() // expect that next vertex will heapify
      S.addOne(u)
      holder.adjacentVertices(u).foreach { v =>
        relax(u, v, weight, distances, predecessors)
      }
      decreaseKeys(priorityQueue)
    }

    (distances, predecessors)
  }

  // scala.collection.mutable.PriorityQueue does not support a decrease-key operation
  private def decreaseKeys(priorityQueue: mutable.PriorityQueue[Int]): Unit =
    if (priorityQueue.nonEmpty) {
      val v = priorityQueue.dequeue()
      priorityQueue.enqueue(v)
    }

  // O(V)
  private def initializeSingleSource(
      source: Int,
      holder: AdjacentHolder
  ): (Array[Int], Array[Option[Int]]) = {
    val distances: Array[Int]            = Array.fill(holder.vertices.size)(10000)
    val predecessors: Array[Option[Int]] = Array.fill(holder.vertices.size)(Option.empty[Int])

    distances.update(source, 0)

    (distances, predecessors)
  }

  private def relax(
      u: Int,
      v: Int,
      weightUV: (Int, Int) => Int,
      distances: Array[Int],
      predecessors: Array[Option[Int]]
  ): Unit =
    if (distances(v) > distances(u) + weightUV(u, v)) {
      distances.update(v, distances(u) + weightUV(u, v))
      predecessors.update(v, Some(u))
    }

}

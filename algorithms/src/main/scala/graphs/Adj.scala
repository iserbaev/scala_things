package graphs

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

/** Adjacency list representation (список смежности)
  * Для каждой вершины u Adj[u] состоит из всех вершин смежных с u в графе G
  */
case class Adj[V](g: Map[V, List[V]]) {
  import Adj._
  import Color._

  def adjacent(u: V, v: V): Boolean =
    g(u).contains(v)

  val values: List[V] = g.keys.toList

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

  def bfs(s: V): BFSMeta[V] = {
    val colors    = mutable.Map.empty[V, Color]
    val distances = mutable.Map.empty[V, Int]
    val parents   = mutable.Map.empty[V, Option[V]]

    values.foreach { v =>
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

      values
        .foreach { v =>
          if (colors(v) == White && adjacent(u, v)) {
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

object Adj {
  case class DFSMeta[V](
    componentsCount: Int,
    components:      Map[V, Int],
    tin:             Map[V, Int],
    tout:            Map[V, Int]
  )

  case class BFSMeta[V](
    colors:    Map[V, Color],
    distances: Map[V, Int],
    parents:   Map[V, Option[V]]
  )

  def build[V](edgesMappings: Map[String, List[V]]): Adj[V] = {

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

    new Adj(gMap.toMap)
  }
}

object TestAdj extends App {
  val adj = Adj.build(
    Map(
      "a" -> List(1, 5),
      "b" -> List(1, 2),
      "c" -> List(2, 4),
      "d" -> List(4, 5),
      "e" -> List(2, 3),
      "f" -> List(3, 4),
      "g" -> List(6, 7),
      "h" -> List()
    )
  )

  println(adj)

  println(adj.bfs(5))

  println(adj.dfs)

  // TODO check for 3 trees
}

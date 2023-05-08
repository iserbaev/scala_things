package graphs

import java.io.{ BufferedReader, InputStreamReader }
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.{ immutable, mutable }

/** Adjacency list representation (список смежности) Для каждой вершины u Adj[u] состоит
  * из всех вершин смежных с u в графе G
  */
case class Adj[V](g: Map[V, Set[V]]) {
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
      components: Map[V, Int],
      tin: Map[V, Int],
      tout: Map[V, Int]
  )

  case class BFSMeta[V](
      colors: Map[V, Color],
      distances: Map[V, Int],
      parents: Map[V, Option[V]]
  )

  def build[V](edgesMappings: Seq[List[V]]): Adj[V] = {

    val gMap = edgesMappings.foldLeft(mutable.Map.empty[V, Set[V]]) { case (gi, pair) =>
      pair match {
        case List(v1, v2) =>
          gi.update(v1, gi.getOrElse(v1, Set.empty[V]) + v2)
          gi.update(v2, gi.getOrElse(v2, Set.empty[V]) + v1)
          gi
        case List(v1) =>
          gi.update(v1, gi.getOrElse(v1, Set.empty[V]))
          gi
        case _ =>
          gi
      }
    }

    new Adj(gMap.toMap)
  }
}

object MainAdj {
  def main(args: Array[String]): Unit = {
    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    val frst             = br.readLine().split(" ")
    val (vCount, eCount) = frst.head.toInt -> frst.last.toInt
    var maxV             = Int.MinValue
    val pairs: immutable.IndexedSeq[List[Int]] = (0 until eCount).map { _ =>
      val arr = br.readLine().split(" ").map(_.toInt).toList

      maxV = math.max(maxV, arr.max)

      arr
    }

    val additional: immutable.IndexedSeq[List[Int]] =
      (0 until math.max(0, vCount - maxV)).map(v => List(v))

    process(pairs ++ additional)

    br.close()
  }

  def process(pairs: IndexedSeq[List[Int]]): Unit = {
    val adj = Adj.build(pairs)
    println(adj.dfs.componentsCount)
  }

  //6 5
  //1 2
  //3 4
  //5 6
  //2 3
  //6 1
}

object MainDistances {
  def main(args: Array[String]): Unit = {
    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    val frst        = br.readLine().split(" ")
    val (_, eCount) = frst.head.toInt -> frst.last.toInt
    val pairs: IndexedSeq[List[Int]] = (0 until eCount).map { _ =>
      br.readLine().split(" ").map(_.toInt).toList
    }

    process(pairs)

    br.close()
  }

  def process(pairs: IndexedSeq[List[Int]]): Unit = {
    val adj = Adj.build(pairs)
    adj.bfs(0).distances.toSeq.sortWith(_._1 < _._1).foreach { case (_, v) =>
      print(s"$v ")
    }
  }

  //6 7
  //0 1
  //1 2
  //2 0
  //3 2
  //4 3
  //4 2
  //5 4

  //0 1 1 2 2 3
}

object TestAdj extends App {
  val adj = Adj.build(
    Seq(
      List(1, 5),
      List(1, 2),
      List(2, 4),
      List(4, 5),
      List(2, 3),
      List(3, 4),
      List(5),
      List(6, 7),
      List(8)
    )
  )

  println(adj)

  println(adj.bfs(5))

  println(adj.dfs)

  // TODO check for 3 trees
}

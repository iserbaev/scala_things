package graphs

sealed trait AdjacentHolder {
  def adjacent(u: Int, v: Int): Boolean
  def adjacentVertices(v: Int): Set[Int]

  def vertices: IndexedSeq[Int]
  def edges: Seq[(Int, Int)]
}

object AdjacentHolder {
  case class AdjMatrix(val vertices: IndexedSeq[Int], val edges: Seq[(Int, Int)], matrix: Array[Array[Int]])
      extends AdjacentHolder {
    // O(1)
    def size: Int = matrix.length

    // O(1)
    def adjacent(u: Int, v: Int): Boolean =
      matrix(u)(v) == 1

    // O(n)
    def adjacentVertices(v: Int): Set[Int] =
      matrix(v).zipWithIndex.collect { case (v, idx) if v > 0 => idx }.toSet

    // O(n)
    def loopsCount: Int =
      (0 until size).count(i => matrix(i)(i) == 1)

    // O(m)
    def edgesCount: Int =
      edges.size

    // O(n)
    def degrees: Array[Int] = matrix.zipWithIndex.map { case (row, idx) =>
      if (row(idx) == 1) row.sum + 1 else row.sum
    }

    // O(n^2)

    def sourcesAndDrainsInOrientedGraph: (Array[Int], Array[Int]) = {
      val in  = Array.fill(size)(0)
      val out = Array.fill(size)(0)

      (for {
        i <- vertices
        j <- vertices
      } yield {
        if (adjacent(i, j)) {
          out.update(i, 1)
          in.update(j, 1)
        }
      }): Unit

      (in, out)
    }

    // O(n^2)
    def sourcesAndDrainsCountInOrientedGraph: (Int, Int) = {
      val (in, out) = sourcesAndDrainsInOrientedGraph
      (in.count(_ == 0), out.count(_ == 0))
    }
  }

  /** Adjacency list representation (список смежности) Для каждой вершины u Adj[u] состоит
    * из всех вершин смежных с u в графе G
    */
  class AdjList(val vertices: IndexedSeq[Int], val edges: Seq[(Int, Int)], val underlying: Map[Int, Set[Int]])
      extends AdjacentHolder {
    def adjacent(v1: Int, v2: Int): Boolean =
      underlying(v1).contains(v2)

    def adjacentVertices(v: Int): Set[Int] = underlying(v)

    def degrees: Seq[Int] = vertices.map { v =>
      val edges = underlying
        .getOrElse(v, Set.empty[Int])

      edges.size
    }
  }

  object AdjList {
    def buildNonOriented(vertices: Seq[Int], edges: Seq[(Int, Int)]): AdjList = {

      val edgesMap = edges.foldLeft(scala.collection.mutable.Map.empty[Int, Set[Int]]) { case (acc, (e1, e2)) =>
        acc.update(e1, acc.getOrElse(e1, Set.empty[Int]) + e2)
        acc.update(e2, acc.getOrElse(e2, Set.empty[Int]) + e1)
        acc
      }

      vertices.foreach(v => edgesMap.update(v, edgesMap.getOrElse(v, Set.empty[Int])))

      new AdjList(vertices.toIndexedSeq, edges.sorted, edgesMap.toMap)
    }

    def buildNonOriented(edges: Seq[(Int, Int)]): AdjList =
      buildNonOriented(edges.indices, edges)

    def buildOriented(vertices: Seq[Int], edges: Seq[(Int, Int)]): AdjList = {
      val edgesMap = edges.foldLeft(scala.collection.mutable.Map.empty[Int, Set[Int]]) { case (acc, (e1, e2)) =>
        acc.update(e1, acc.getOrElse(e1, Set.empty[Int]) + e2)
        acc
      }

      vertices.foreach(v => edgesMap.update(v, edgesMap.getOrElse(v, Set.empty[Int])))

      new AdjList(vertices.toIndexedSeq, edges.sorted, edgesMap.toMap)
    }
  }

}

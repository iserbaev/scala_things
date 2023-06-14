package graphs

sealed trait AdjacentHolder {
  def adjacent(u: Int, v: Int): Boolean
  def adjacentVertices(v: Int): Set[Int]

  def vertices: IndexedSeq[Int]
}

object AdjacentHolder {
  class AdjMatrix(val vertices: IndexedSeq[Int], matrix: Array[Array[Int]]) extends AdjacentHolder {
    def size: Int = matrix.length

    def adjacent(u: Int, v: Int): Boolean =
      matrix(u)(v) == 1

    def adjacentVertices(v: Int): Set[Int] =
      matrix(v).zipWithIndex.collect { case (v, idx) if v == 1 => idx }.toSet

    def loopsCount: Int =
      (0 until size).count(i => matrix(i)(i) == 1)

    def edgesCount: Int =
      matrix.map(_.sum).sum

    def degrees: Array[Int] = matrix.zipWithIndex.map { case (row, idx) =>
      if (row(idx) == 1) row.sum + 1 else row.sum
    }

     def sourcesAndDrainsInOrientedGraph: (Array[Int], Array[Int]) = {
       val in = Array.fill(size)(0)
       val out = Array.fill(size)(0)

       for {
         i <- vertices
         j <- vertices
       } yield {
         if (matrix(i)(j) == 1) {
           out.update(i, 1)
           in.update(j, 1)
         }
       }

       (in, out)
     }

    def sourcesAndDrainsCountInOrientedGraph: (Int, Int) = {
      val (in, out) = sourcesAndDrainsInOrientedGraph
      (in.count(_ == 0), out.count(_ == 0))
    }

    def toAdjList: AdjList = {
      val edges = Seq.newBuilder[(Int, Int)]
      for {
        i <- (0 until size)
        j <- (i until size)
      } yield {
        if (adjacent(i, j)) edges.+=((i + 1, j + 1))
      }

      AdjList(edges.result())
    }
  }

  object AdjMatrix {
    def apply(raw: Array[Array[Int]]): AdjMatrix =
      new AdjMatrix(raw.indices, raw)
  }

  /** Adjacency list representation (список смежности) Для каждой вершины u Adj[u] состоит
    * из всех вершин смежных с u в графе G
    */
  class AdjList(val vertices: IndexedSeq[Int], val edges: Seq[(Int, Int)], underlying: Map[Int, Set[Int]]) extends AdjacentHolder {
    def adjacent(v1: Int, v2: Int): Boolean =
      underlying(v1).contains(v2)

    def adjacentVertices(v: Int): Set[Int] = underlying(v)
  }

  object AdjList {
    def apply(vertices: Seq[Int], edges: Seq[(Int, Int)]): AdjList = {

      val edgesMap = edges.foldLeft(scala.collection.mutable.Map.empty[Int, Set[Int]]) { case (acc, (e1, e2)) =>
        acc.update(e1, acc.getOrElse(e1, Set.empty[Int]) + e2)
        acc.update(e2, acc.getOrElse(e2, Set.empty[Int]) + e1)
        acc
      }

      vertices.foreach(v => edgesMap.update(v, edgesMap.getOrElse(v, Set.empty[Int])))

      new AdjList(vertices.toIndexedSeq, edges, edgesMap.toMap)
    }

    def apply(edges: Seq[(Int, Int)]): AdjList =
      apply(edges.indices, edges)
  }

}

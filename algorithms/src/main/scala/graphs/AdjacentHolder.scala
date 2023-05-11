package graphs

import scala.collection.mutable

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
  }

  object AdjMatrix {
    def apply(raw: Array[Array[Int]]): AdjMatrix =
      new AdjMatrix(raw.indices, raw)
  }

  /** Adjacency list representation (список смежности) Для каждой вершины u Adj[u] состоит
    * из всех вершин смежных с u в графе G
    */
  class AdjList(underlying: Map[Int, Set[Int]]) extends AdjacentHolder {
    def adjacent(v1: Int, v2: Int): Boolean =
      underlying(v1).contains(v2)

    def adjacentVertices(v: Int): Set[Int] = underlying(v)

    val vertices: IndexedSeq[Int] = underlying.keys.toIndexedSeq
  }

  object AdjList {
    def apply(edgesMappings: Seq[List[Int]]): AdjList = {

      val gMap = edgesMappings.foldLeft(mutable.Map.empty[Int, Set[Int]]) { case (gi, pair) =>
        pair match {
          case List(v1, v2) =>
            gi.update(v1, gi.getOrElse(v1, Set.empty[Int]) + v2)
            gi.update(v2, gi.getOrElse(v2, Set.empty[Int]) + v1)
            gi
          case List(v1) =>
            gi.update(v1, gi.getOrElse(v1, Set.empty[Int]))
            gi
          case _ =>
            gi
        }
      }

      new AdjList(gMap.toMap)
    }
  }

}

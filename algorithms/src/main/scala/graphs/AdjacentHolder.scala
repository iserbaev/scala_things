package graphs

import spire.ClassTag

import scala.collection.mutable
sealed trait AdjacentHolder[V] {
  def adjacent(u: V, v: V): Boolean
  def adjacentVertices(v: V): Set[V]
  def vertices: IndexedSeq[V]

}

object AdjacentHolder {
  case class Mapping[V](vertexIndex: Map[V, Int], indexVertex: Map[Int, V])
  object Mapping {
    def apply[V](vertices: IndexedSeq[V]): Mapping[V] = {
      val (vertexIndex: Map[V, Int], indexVertex: Map[Int, V]) = {
        val (vi, iv) = vertices.zipWithIndex.foldLeft((Map.newBuilder[V, Int], Map.newBuilder[Int, V])) {
          case ((vi, iv), (vertex, idx)) =>
            (vi.addOne((vertex, idx)), iv.addOne((idx, vertex)))
        }
        (vi.result(), iv.result())
      }
      new Mapping(vertexIndex, indexVertex)
    }
  }

  case class EdgesList[V: ClassTag](vertices: IndexedSeq[V], edges: List[(V, V)]) {
    def toAdjacentList: AdjList[V] = {
      val map = mutable.Map.empty[V, Set[V]]
      vertices.foreach(v => map.update(v, Set.empty))
      val gMap = edges.foldLeft(map) { case (gi, (v1, v2)) =>
        gi.update(v1, gi.getOrElse(v1, Set.empty[V]) + v2)
        gi.update(v2, gi.getOrElse(v2, Set.empty[V]) + v1)
        gi
      }

      new AdjList[V](gMap.toMap)
    }

    def toAdjacentMatrix: AdjMatrix[V] = {
      val size    = vertices.size
      val mapping = Mapping(vertices)
      val matrix = edges.foldLeft(Array.ofDim[Int](size, size)) { case (matrix, (v1, v2)) =>
        matrix(mapping.vertexIndex(v1))(mapping.vertexIndex(v2)) = 1
        matrix
      }

      new AdjMatrix(vertices, matrix)
    }
  }

  object EdgesList {
    def apply(edges: Seq[List[Int]]): EdgesList[Int] =
      new EdgesList(edges.indices, edges.zipWithIndex.flatMap { case (edged, idx) => edged.map(_ -> idx) }.toList)
  }

  class AdjMatrix[V: ClassTag](val vertices: IndexedSeq[V], matrix: Array[Array[Int]]) extends AdjacentHolder[V] {
    private val mapping = Mapping(vertices)

    def adjacent(u: V, v: V): Boolean =
      matrix(mapping.vertexIndex(u))(mapping.vertexIndex(v)) == 1

    def adjacentVertices(v: V): Set[V] =
      matrix(mapping.vertexIndex(v)).zipWithIndex
        .filter(_._1 == 1)
        .map { case (_, idx) => mapping.indexVertex(idx) }
        .toSet
  }

  /** Adjacency list representation (список смежности) Для каждой вершины u Adj[u] состоит
    * из всех вершин смежных с u в графе G
    */
  class AdjList[V](underlying: Map[V, Set[V]]) extends AdjacentHolder[V] {
    def adjacent(v1: V, v2: V): Boolean =
      underlying(v1).contains(v2)

    def adjacentVertices(v: V): Set[V] = underlying(v)

    val vertices: IndexedSeq[V] = underlying.keys.toIndexedSeq
  }

}

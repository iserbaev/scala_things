package graphs

object ShortestPathLee {
  def main(args: Array[String]): Unit = {
    val (m, start, end) = readMatrix
    val matrix = AdjacentHolder.AdjMatrix(m.indices, Seq.empty, m)

    val result = GraphsProcessor.shortestPathLee(start, end, matrix)

    println(result)
  }

  def readMatrix = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    val size = br.readLine().toInt

    val matrix = (0 until size).map { _ =>
      br.readLine().split(" ").map(_.toInt)
    }.toArray

    val startEnd = br.readLine().split(" ").map(_.toInt)

    br.close()

    (matrix, startEnd.head - 1, startEnd.last - 1)
  }
}

object DijkstraTest {
  def main(args: Array[String]): Unit = {
    val (matrix, vertices, edges, source, end) = readMatrix
    val holder = AdjacentHolder.AdjMatrix(vertices, edges, matrix)

    val weight: (Int, Int) => Int = (u,v) => matrix(u)(v)

    val (distances, _) = GraphsProcessor.dijkstra(holder, weight, source)

    println(distances(end))
  }

  def readMatrix = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    val List(n,source,end) = br.readLine().split(" ", 3).map(_.toInt).toList

    val edges = Seq.newBuilder[(Int, Int)]
    val vertices = (0 until n).toIndexedSeq

    val matrix = vertices.map { u =>
      val row = br.readLine().split(" ").map(_.toInt)
      row.zipWithIndex.foreach{ case (weight, v) =>
        if (weight > 0) {
          edges.+=((u, v))
        }
      }
      row
    }.toArray

    br.close()

    (matrix, vertices, edges.result(), source - 1, end - 1)
  }
}

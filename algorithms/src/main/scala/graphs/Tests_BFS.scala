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

    val edges = (0 until size).map { _ =>
      br.readLine().split(" ").map(_.toInt)
    }.toArray

    val startEnd = br.readLine().split(" ").map(_.toInt)

    br.close()

    (edges, startEnd.head - 1, startEnd.last - 1)
  }
}

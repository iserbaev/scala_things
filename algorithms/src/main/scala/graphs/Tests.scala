package graphs

import graphs.LoopsCount.readMatrix

object MainAdj {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    val (vertexCount, edgeCount) = {
      val tuple = br.readLine().split(" ")
      tuple.head.toInt -> tuple.last.toInt
    }

    val vertices = 1 to vertexCount
    val edges: IndexedSeq[List[Int]] = (0 until edgeCount).map { _ =>
      br.readLine().split(" ").map(_.toInt).toList
    }

    val adj = AdjacentHolder.AdjList(vertices, edges)
    println(GraphsProcessor.dfs(adj).componentsCount)

    br.close()
  }
}

object MainDistances {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    val (vertexCount, edgeCount) = {
      val tuple = br.readLine().split(" ")
      tuple.head.toInt -> tuple.last.toInt
    }

    val vertices = 0 until vertexCount
    val edges: IndexedSeq[List[Int]] = (0 until edgeCount).map { _ =>
      br.readLine().split(" ").map(_.toInt).toList
    }

    val adj       = AdjacentHolder.AdjList(vertices, edges)
    val distances = GraphsProcessor.bfs(0, adj).distances
    val result = distances.toSeq.sortBy(_._1).map(_._2)
    println(result.mkString(" "))

    br.close()
  }
}

object DistancesTest extends App {
  def parse(s: String): AdjacentHolder.AdjList = {
    val res = scala.io.Source.fromString(s).getLines().toIndexedSeq
    val (vertexCount, _) = {
      val tuple = res.head.split(" ")
      tuple.head.toInt -> tuple.last.toInt
    }

    val vertices = 0 until vertexCount
    val edges: IndexedSeq[List[Int]] = res.tail.map { line =>
      line.split(" ").map(_.toInt).toList
    }

    AdjacentHolder.AdjList(vertices, edges)
  }
  def test(vertexCount: Int, edges: Seq[List[Int]], expected: Seq[Int]): Unit = {
    val vertices = (0 until vertexCount).toSeq
    val adj      = AdjacentHolder.AdjList(vertices, edges)
    test(adj, expected)
  }

  def test(adjacentHolder: AdjacentHolder, expected: Seq[Int]): Unit = {
    val distances = GraphsProcessor.bfs(0, adjacentHolder).distances
    val result = distances.toSeq.sortBy(_._1).map(_._2)
    if (result != expected)
      println(s"${result.mkString("[", ",", "]")} != ${expected.mkString("[", ",", "]")}")
  }

  def test(s: String, expected: String): Unit =
    test(parse(s), expected.split(" ").map(_.toInt).toSeq)

  test(
    s"""6 7
       |0 1
       |1 2
       |2 0
       |3 2
       |4 3
       |4 2
       |5 4""".stripMargin,
    "0 1 1 2 2 3"
  )

  test(
    s"""11 13
       |0 1
       |1 2
       |1 3
       |1 4
       |2 5
       |3 8
       |4 3
       |4 5
       |5 6
       |5 7
       |5 10
       |7 8
       |8 9""".stripMargin,
    "0 1 2 2 2 3 4 4 3 4 4"
  )

  test("1 0", "0")


  test(
    s"""5 10
       |0 1
       |0 2
       |0 3
       |0 4
       |1 2
       |1 3
       |1 4
       |2 3
       |2 4
       |3 4""".stripMargin, "0 1 1 1 1")

  test(
    s"""8 8
       |0 3
       |1 3
       |1 2
       |1 4
       |2 6
       |4 5
       |5 6
       |6 7""".stripMargin,
    "0 2 3 1 3 4 4 5"
  )

  test(
    s"""12 13
       |0 1
       |0 2
       |1 3
       |2 3
       |3 4
       |4 5
       |4 6
       |5 7
       |5 8
       |6 7
       |6 9
       |6 11
       |8 10""".stripMargin,
    "0 1 1 2 3 4 4 5 5 5 6 5"
  )

  test(
    s"""12 14
       |0 1
       |0 2
       |1 3
       |2 3
       |3 4
       |4 5
       |4 6
       |5 7
       |5 8
       |6 7
       |6 9
       |6 11
       |8 10""".stripMargin,
    "0 1 1 2 3 3 4 4 2 5 1 5"
  )
}

////1 1 1 1 0
////1 0 1 1 1
////1 1 0 1 1
////1 1 1 1 1
////0 1 1 1 0
object BuildMatrixTest {
  def main(args: Array[String]): Unit = {

    val matrix = AdjacentHolder.AdjMatrix(
      Array(
        Array(1, 1, 1, 1, 0),
        Array(1, 0, 1, 1, 1),
        Array(1, 1, 0, 1, 1),
        Array(1, 1, 1, 1, 1),
        Array(0, 1, 1, 1, 0)
      )
    )

    assert(matrix.loopsCount == 2)
  }
}

object LoopsCount {
  def main(args: Array[String]): Unit = {
    val adjacentHolder = AdjacentHolder.AdjMatrix(readMatrix)

    println(adjacentHolder.loopsCount)


  }

  def readMatrix: Array[Array[Int]] = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    val size = br.readLine().toInt

    val edges = (0 until size).map { _ =>
      br.readLine().split(" ").map(_.toInt)
    }.toArray

    br.close()

    edges
  }
}

object EdgesCount {
  def main(args: Array[String]): Unit = {
    val adjacentHolder = AdjacentHolder.AdjMatrix(readMatrix)

    println(adjacentHolder.edgesCount)
  }
}

object EdgesList {
  def main(args: Array[String]): Unit = {
    val adjacentHolder = AdjacentHolder.AdjMatrix(readMatrix)

    val even = adjacentHolder.degrees.count(_ % 2 == 0)

    println(s"${even} ${adjacentHolder.vertices.length - even}")
  }
}

object TestAdj extends App {
  val adj = AdjacentHolder.AdjList(
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

  println(GraphsProcessor.bfs(5, adj))

  println(GraphsProcessor.dfs(adj))

  // TODO check for 3 trees
}

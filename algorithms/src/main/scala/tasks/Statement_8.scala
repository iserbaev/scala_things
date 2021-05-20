package tasks

import structures.DisjointSetRank

import java.io.{BufferedReader, InputStreamReader}

object Main8 {
  def readTuple: (Int, Int) = {
    val t = scala.io.StdIn.readLine().split(" ")
    require(t.length == 2)
    t.head.toInt -> t.last.toInt
  }

  def main(args: Array[String]) = {
    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    val m = br.readLine().split(" ").last.toInt
    val sizes:      Array[Int]      = Stream.continually(br.read()).toArray
    val mergeTasks: Seq[(Int, Int)] = (1 to m).map(_ => readTuple)

    val dse: DisjointSetRank = process(sizes, mergeTasks)

    dse.result.foreach(println)
  }

  def process(
    sizes:      Array[Int],
    mergeTasks: Seq[(Int, Int)]
  ): DisjointSetRank = {
    val dse = new DisjointSetRank()
    sizes.zipWithIndex.foreach { case (a, index) => dse.makeSet(index + 1, a) }
    mergeTasks.foreach {
      case (i, j) =>
        dse.union(i, j)
    }
    dse
  }
}

object Test8 extends App {
  def test(
    sizes:      Array[Int],
    mergeTasks: Seq[(Int, Int)],
    expected:   Seq[Int]
  ): Unit = {
    val result = Main8.process(sizes, mergeTasks).result

    expected.zipWithIndex.foreach {
      case (a, index) =>
        require(result(index) == a, s"${result(index)} != $a")
    }
  }

  test(
    Array(1, 1, 1, 1, 1),
    Array(
      (3, 5),
      (2, 4),
      (1, 4),
      (5, 4),
      (5, 3)
    ),
    Seq(2, 2, 3, 5, 5)
  )

  test(
    Array(6, 5, 3, 7),
    Array(
      (1, 4),
      (2, 1),
      (3, 2)
    ),
    Seq(13, 18, 21)
  )

  test(
    Array(2, 11, 5, 1, 7, 3),
    Array(
      (6, 6),
      (1, 2),
      (6, 5),
      (5, 4),
      (4, 3),
      (3, 1)
    ),
    Seq(11, 13, 13, 13, 16, 29)
  )

}

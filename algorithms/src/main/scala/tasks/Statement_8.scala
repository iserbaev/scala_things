package tasks

import structures.DisjointSetRank

object Main8 {
  import java.io.{BufferedReader, InputStreamReader}
  import java.util.StringTokenizer
  import scala.collection.mutable.ArrayBuffer

  def readTuple(s: String): (Int, Int) = {
    val t = new StringTokenizer(s)
    t.nextToken().toInt -> t.nextToken().toInt
  }

  def readSeq(s: String): IndexedSeq[Int] = {
    val t      = new StringTokenizer(s)
    val buffer = new ArrayBuffer[Int]()

    while (t.hasMoreTokens) {
      buffer.append(t.nextToken().toInt)
    }
    buffer
  }

  def main(args: Array[String]) = {
    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    val (_, m) = readTuple(br.readLine())
    val sizes: IndexedSeq[Int] = readSeq(br.readLine())
    val mergeTasks: Seq[(Int, Int)] =
      (1 to m).map(_ => readTuple(br.readLine()))

    br.close()

    val dse: DisjointSetRank = process(sizes, mergeTasks)

    dse.result.foreach(println)
  }

  def process(
    sizes:      IndexedSeq[Int],
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

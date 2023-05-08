package tasks

import structures.DisjointSetRank

object Main8 {
  import java.io.{ BufferedReader, InputStreamReader }
  import java.util.StringTokenizer
  import scala.collection.mutable.ArrayBuffer

  def readTuple(s: String): (Int, Int) = {
    val t = new StringTokenizer(s)
    t.nextToken().toInt -> t.nextToken().toInt
  }

  def readSeq(s: String): Seq[Int] = {
    val t      = new StringTokenizer(s)
    val buffer = new ArrayBuffer[Int]()

    buffer.append(0)
    while (t.hasMoreTokens) {
      buffer.append(t.nextToken().toInt)
    }
    buffer.toSeq
  }

  def main(args: Array[String]) = {
    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    val (_, m)     = readTuple(br.readLine())
    val sizes      = readSeq(br.readLine())
    val mergeTasks = (1 to m).map(_ => readTuple(br.readLine()))

    br.close()

    val dse: DisjointSetRank = process(sizes, mergeTasks)

    dse.result.foreach(println)
  }

  def process(
      sizes: Seq[Int],
      mergeTasks: Seq[(Int, Int)]
  ): DisjointSetRank = {
    val dse = DisjointSetRank(sizes)
    (1 until sizes.length).foreach { index =>
      dse.makeSet(index)
    }
    mergeTasks.foreach { case (i, j) =>
      dse.union(i, j)
    }
    dse
  }
}

object Test8 extends App {
  import scala.util.Random

  def test(
      sizes: Seq[Int],
      mergeTasks: Seq[(Int, Int)],
      expected: Seq[Int]
  ): Long = {
    val start  = System.currentTimeMillis()
    val result = Main8.process(sizes, mergeTasks).result

    expected.zipWithIndex.foreach { case (a, index) =>
      require(result(index) == a, s"${result(index)} != $a")
    }
    val end = System.currentTimeMillis()

    end - start
  }

  test(
    Seq(0, 1, 1, 1, 1, 1),
    Seq(
      (3, 5),
      (2, 4),
      (1, 4),
      (5, 4),
      (5, 3)
    ),
    Seq(2, 2, 3, 5, 5)
  )

  test(
    Seq(0, 6, 5, 3, 7),
    Seq(
      (1, 4),
      (2, 1),
      (3, 2)
    ),
    Seq(13, 18, 21)
  )

  test(
    Seq(0, 2, 11, 5, 1, 7, 3),
    Seq(
      (6, 6),
      (1, 2),
      (6, 5),
      (5, 4),
      (4, 3),
      (3, 1)
    ),
    Seq(11, 13, 13, 13, 16, 29)
  )

  performanceTest(10000, 1000, 10)
  performanceTest(100000, 10000, 100)

  private def performanceTest(n: Int, r: Int, mult: Int): Unit = {
    val seed = new Random(123456)
    val duration = (0 to mult).map { _ =>
      test(
        (0 to n).map(_ => r).toSeq,
        (0 until n).map(x => (x + 1) -> (seed.nextInt(x + 1).abs + 1)).toSeq,
        Seq.empty
      )
    }.sum

    println(s"duration = $duration MS")
  }

}

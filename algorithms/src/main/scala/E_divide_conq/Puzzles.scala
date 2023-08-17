package E_divide_conq

object BinarySearch {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    br.readLine().toInt // n
    val k = br.readLine().split(" ").map(_.toLong).zipWithIndex.toMap

    br.readLine().toInt // m
    val q = br.readLine().split(" ").map(_.toLong)

    val res = q.map(l => k.getOrElse(l, -1))

    println(res.mkString(" "))
  }

}

object BinarySearchWithDuplicates {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    br.readLine().toInt // n
    val k = br
      .readLine()
      .split(" ")
      .foldLeft((Map.empty[Int, Int], 0)) { case ((acc, idx), v) =>
        acc.get(v.toInt).map(_ => (acc, idx + 1)).getOrElse((acc.updated(v.toInt, idx), idx + 1))
      }
      ._1

    br.readLine().toInt // m
    val q = br.readLine().split(" ").map(_.toInt)
    br.close()

    val res = q.map(l => k.getOrElse(l, -1))

    println(res.mkString(" "))
  }

}

object MajorityElement {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    val n = br.readLine().toInt // n
    val k = br.readLine().split(" ").map(_.toInt).groupBy(identity)

    br.close()

    println(k.count(_._2.length > n / 2))
  }

}

object PointsCover {
  def main(args: Array[String]): Unit = {
    val n = scala.io.StdIn.readLine().split(" ").map(_.toInt).head

    val startsBuilder = IndexedSeq.newBuilder[Int]
    val endsBuilder   = IndexedSeq.newBuilder[Int]

    (1 to n).foreach { _ =>
      val arr   = scala.io.StdIn.readLine().split(" ").map(_.toInt)
      val start = arr.head
      val end   = arr.last
      startsBuilder.+=(start)
      endsBuilder.+=(end)
    }
    val points = scala.io.StdIn.readLine().split(" ").map(_.toInt)
    val starts = startsBuilder.result()
    val ends   = endsBuilder.result()

    val result = points.map { p =>
      val ss = starts.search(p)
      val es = ends.search(p)

      ss.insertionPoint - es.insertionPoint
    }

    println(result.mkString(" "))
  }
}

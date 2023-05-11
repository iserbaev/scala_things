package graphs

import graphs.AdjacentHolder.EdgesList

import java.io.{ BufferedReader, InputStreamReader }

object MainAdj {
  def main(args: Array[String]): Unit = {
    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    val frst             = br.readLine().split(" ")
    val (vCount, eCount) = frst.head.toInt -> frst.last.toInt
    var maxV             = Int.MinValue
    val pairs: IndexedSeq[List[Int]] = (0 until eCount).map { _ =>
      val arr = br.readLine().split(" ").map(_.toInt).toList

      maxV = math.max(maxV, arr.max)

      arr
    }

    val additional: IndexedSeq[List[Int]] =
      (0 until math.max(0, vCount - maxV)).map(v => List(v))

    process(pairs ++ additional)

    br.close()
  }

  def process(pairs: Seq[List[Int]]): Unit = {
    val adj = EdgesList(pairs).toAdjacentList
    println(GraphsProcessor.dfs(adj).componentsCount)
  }

  //6 5
  //1 2
  //3 4
  //5 6
  //2 3
  //6 1
}

object MainDistances {
  def main(args: Array[String]): Unit = {
    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    val frst        = br.readLine().split(" ")
    val (_, eCount) = frst.head.toInt -> frst.last.toInt
    val pairs: IndexedSeq[List[Int]] = (0 until eCount).map { _ =>
      br.readLine().split(" ").map(_.toInt).toList
    }

    process(pairs)

    br.close()
  }

  def process(pairs: Seq[List[Int]]): Unit = {
    val adj = EdgesList(pairs).toAdjacentList
    GraphsProcessor.bfs(0, adj).distances.toSeq.sortWith(_._1 < _._1).foreach { case (_, v) =>
      print(s"$v ")
    }
  }

  //6 7
  //0 1
  //1 2
  //2 0
  //3 2
  //4 3
  //4 2
  //5 4

  //0 1 1 2 2 3
}

object LoopsCount {
  def main(args: Array[String]): Unit = {
    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    val size = br.readLine().toInt

    var count = 0

    (0 until size).foreach { i =>
      val row = br.readLine().split(" ").map(_.toInt)

      if (row(i) == 1) count += 1
    }

    println(count)

    br.close()
  }
}

object EdgesCount {
  def main(args: Array[String]): Unit = {
    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    val size = br.readLine().toInt

    val count = (0 until size).map { _ =>
      val row = br.readLine().split(" ").map(_.toInt)

      row.sum
    }.sum

    println(count)

    br.close()
  }
}

object TestAdj extends App {
  val adj = EdgesList(
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
  ).toAdjacentList

  println(adj)

  println(GraphsProcessor.bfs(5, adj))

  println(GraphsProcessor.dfs(adj))

  // TODO check for 3 trees
}

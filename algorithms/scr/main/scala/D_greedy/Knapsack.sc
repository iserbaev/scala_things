
import scala.collection.mutable.ListBuffer
import scala.io.StdIn
object Main {
  def solve1(cws: Seq[(Int,Int)],W: Int) = {
    @scala.annotation.tailrec
    def fill(cws: Seq[(Int, Int)], W: Int, acc: ListBuffer[(Double,Int)]): ListBuffer[(Double, Int)] = W match {
      case x if x < 0 =>
        throw sys.error("W less than 0")
      case 0 =>
        acc
      case _ if cws.isEmpty =>
        acc
      case _ =>
        val (c,w) = cws.head
        val part = if (W < w) {
          val a = (W.toDouble / w) * c
          (a,W)
        } else {
          (c.toDouble,w)
        }
        acc.append((part._1, part._2))
        fill(cws.tail, W - part._2, acc)
    }

    fill(
      cws.sortBy{ case (c,w) => - c.toDouble / w},
      W,
      ListBuffer()
    )
  }
  def main(args: Array[String]): Unit = {
    val ar = StdIn.readLine().split(" ")
    val n = ar.head.toInt
    val W = ar.last.toInt

    val cws = (1 to n).map { _ =>
      val arr = StdIn.readLine().split(" ").map(_.toInt)
      arr.head -> arr.last
    }

    val result = solve1(cws, W).map(_._1).sum

    println(result)
  }
}

val ch = Seq((3316,1601), (5375,8940), (2852,6912), (3336,9926), (1717, 8427))

Main.solve1(ch, 9022).map(_._1).sum
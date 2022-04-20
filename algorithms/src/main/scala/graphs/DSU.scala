package graphs

import java.io.{BufferedReader, InputStreamReader}
import scala.collection.mutable

object DSU {
  private val parent = Array.fill(1000)(0)

  def findTopmostParent(index: Int): Int = {
    def recur(i: Int): Int = parent(i) match {
      case ii if ii == i =>
        ii
      case ii =>
        val res = recur(ii)
        parent.update(i, res)
        res
    }

    recur(index)
  }

  private def connect(a: Int, b: Int): Unit = {
    val pa = findTopmostParent(a)
    val pb = findTopmostParent(b)

    if (pa != pb) {
      parent.update(pb, pa)
    }
  }

  private def connectedComponents(n: Int): Int = {
    val s = mutable.Set[Int]()

    (0 until n).foreach(i => s.add(findTopmostParent(i)))

    s.size
  }

  def getConnectedComponents(n: Int, edges: Seq[Seq[Int]]): Int = {

    // Setting parent to itself
    (0 to n).foreach(i => parent.update(i, i))

    // Traverse all edges
    edges.indices.foreach(i => connect(edges(i)(0), edges(i)(1)))

    // Print answer
    connectedComponents(n)
  }

}

object MainDSU {
  def main(args: Array[String]): Unit = {
    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    val frst             = br.readLine().split(" ")
    val (vCount, eCount) = frst.head.toInt -> frst.last.toInt

    val seq: Seq[Seq[Int]] = (0 until eCount).map { _ =>
      val arr = br.readLine().split(" ").map(_.toInt).toSeq

      arr
    }

    val c = DSU.getConnectedComponents(vCount, seq)

    println(c)

    br.close()
  }
}

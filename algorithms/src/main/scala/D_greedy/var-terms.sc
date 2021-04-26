import scala.collection.mutable.ListBuffer
import scala.io.StdIn
object Main {
  def solve(sum: Int): (Int,Seq[Int]) = {
    def recur(sumR: Int, acc: ListBuffer[Int], prev: Int): ListBuffer[Int] = (sumR, prev) match {
      case (s, _) if s == 0 =>
        acc
      case (s, _) if s < 3 && acc.isEmpty =>
        ListBuffer(s)
      case (s, p) if (p + 1) > s =>
        throw sys.error(s"(prev + 1) > sumR. prev=$prev, sumR=$sumR")
      case (s, p) =>
        val p1 = p + 1
        val p2 = p + 2
        if ((s - p1) < p2) {
          acc.append(s)
          recur(0, acc, s)
        } else {
          acc.append(p1)
          recur(s - p1, acc, p1)
        }
    }
    if (sum < 3) {
      (1, Seq(sum))
    } else {
      val seq = recur(sum - 1, ListBuffer(1), 1).toSeq
      (seq.size, seq.sorted)
    }
  }
  def main(args: Array[String]): Unit = {
    val sum = StdIn.readInt()

    val (n,range) = solve(sum)

    println(n)
    println(range.mkString(" "))
  }
}

import Main.solve

val tests = List(
  1 -> Seq(1),
  2 -> Seq(2),
  3 -> Seq(1,2),
  4 -> Seq(1,3),
  5 -> Seq(1,4),
  6 -> Seq(1,2,3),
  7 -> Seq(1,2,4),
  8 -> Seq(1,2,5),
  9 -> Seq(1,2,6),
  10 -> Seq(1,2,3,4),
  14 -> Seq(1,2,3,8))

tests.foreach{ case (sum,res) => assert(res == solve(sum)._2 && res.size == solve(sum)._1, s"res != solve(sum)._2. res=$res. sum=$sum. solve(sum)=${solve(sum)}")}



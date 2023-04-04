
import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Main {
  def solve1(costWeightTuples: Seq[(Int, Int)], availableWeight: Int) = {
    @scala.annotation.tailrec
    def fill(
      costWithWeights: Seq[(Int, Int)],
      knapsackWeight:  Int,
      acc:             ListBuffer[(Double, Int)]
    ): ListBuffer[(Double, Int)] = knapsackWeight match {
      case x if x < 0 =>
        throw sys.error("W less than 0")
      case 0 =>
        acc
      case _ if costWithWeights.isEmpty =>
        acc
      case _ =>
        val (cost, weight) = costWithWeights.head
        val (resultCost, resultWeight) =
          if (knapsackWeight < weight) {
            val partialCost = (knapsackWeight.toDouble / weight) * cost
            (partialCost, knapsackWeight)
          } else {
            (cost.toDouble, weight)
          }

        acc.append((resultCost, resultWeight))
        fill(costWithWeights.tail, knapsackWeight - resultWeight, acc)
    }

    fill(
      costWeightTuples.sortBy { case (c, w) => -c.toDouble / w },
      availableWeight,
      ListBuffer()
    )
  }
  def main(args: Array[String]): Unit = {
    val ar     = scala.io.StdIn.readLine().split(" ")
    val n      = ar.head.toInt
    val weight = ar.last.toInt

    val costWithWeights = (1 to n).map { _ =>
      val arr = StdIn.readLine().split(" ").map(_.toInt)
      arr.head -> arr.last
    }

    val resultCost = solve1(costWithWeights, weight).map(_._1).sum

    println(resultCost)
  }
}

val ch =
  Seq((3316, 1601), (5375, 8940), (2852, 6912), (3336, 9926), (1717, 8427))

Main.solve1(ch, 9022).map(_._1).sum
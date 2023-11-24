package D_greedy

/** Compute the minimum number of coins needed to change the given value into coins with
  * denominations 1, 5, and 10.
  */
object MoneyChange {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    val n = br.readLine().toInt

    //⌊money/10⌋+⌊(money mod 10)/5⌋+(money mod 5)

    val coins = java.lang.Math.floor(n.toDouble / 10).toInt +
      java.lang.Math.floor((n % 10).toDouble / 5).toInt +
      n % 5

    println(coins)
  }

}

object CarFuel {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    val distance = br.readLine().toInt
    val capacity = br.readLine().toInt
    br.readLine().toInt: Unit
    val stops = br.readLine().split(" ").map(_.toInt) :+ distance

    if (capacity >= distance) println(0)
    else if (capacity == 0) println(-1)
    else {
      val (count, _, _, isPossible) = stops.foldLeft((0, 0, 0, true)) {
        case ((refillCount, prevStop, prev, canContinue), stop) =>
//        println(s"${((refillCount, prevStop, prev, canContinue), stop)}")
          if (canContinue) {
            val diff = capacity - (stop - prevStop)
            if (diff >= 0) (refillCount, prevStop, stop, canContinue)
            else {
              (refillCount + 1, prev, stop, capacity >= (stop - prev))
            }
          } else {
            (refillCount, prevStop, prev, canContinue)
          }
      }

      if (!isPossible) println(-1) else println(count)

    }
  }

}

object MaximumAdvertisementRevenue {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    br.readLine().toInt: Unit
    val prices = br.readLine().split(" ").map(_.toInt).sorted(Ordering.Int.reverse)
    val clicks = br.readLine().split(" ").map(_.toInt).sorted(Ordering.Int.reverse)

    var sum = 0L

    clicks.indices.foreach(i => sum = sum + prices(i) * clicks(i))

    println(sum)
  }
}

object MaximumNumberOfPrizes {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    var n = br.readLine().toInt

    if (n < 3) {
      println(1)
      println(n)
    } else {
      val sequence = Array.newBuilder[Int]
      var lastNum  = 0

      while (n > 0) {
        lastNum += 1
        val nn = n - lastNum

        if (nn >= 0 && n >= lastNum + lastNum + 1) {
          sequence.+=(lastNum)
          n = nn
        } else {
          sequence.+=(n)
          n = 0
        }
      }

      val result = sequence.result()

      println(result.length)
      println(result.mkString(" "))
    }
  }
}

//
object LargestConcatenate {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    br.readLine().toInt: Unit
    val numbers = br.readLine().split(" ").map(_.toInt).toArray

    val ordering = Ordering.fromLessThan[Int] { (l, r) =>
      (l.toString + r.toString).toInt > (r.toString + l.toString).toInt
    }

    val sorted = numbers.sorted(ordering)

    println(sorted.mkString(""))
  }
}

object CookingDinner {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    val n = br.readLine().toInt

    val cf = (1 to n).map { _ =>
      val pair = br.readLine().split(" ").map(_.toLong)

      (pair.head, pair.last)
    }

    val sorted = cf.sortBy(t => t._1 + t._2)(Ordering.Long.reverse)

    var allFresh = true

    var currentF = Long.MaxValue
    var idx      = 0

    while (allFresh && idx < sorted.length) {
      val (c, f) = sorted(idx)
      currentF = math.min(f, currentF - c)
      idx += 1
      allFresh = currentF >= 0
    }

    println(if (allFresh) "Yes" else "No")
  }
}

package leetcode.top150

object MaxProfit3 extends App {
  def maxProfit(prices: Array[Int]): Int =
    prices
      .foldLeft((Int.MaxValue, 0, Int.MaxValue, 0)) { case ((buy1, profit1, buy2, profit2), price) =>
        val b1 = math.min(buy1, price)
        val p1 = math.max(profit1, price - buy1)
        val b2 = math.min(buy2, price - p1)
        val p2 = math.max(profit2, price - b2)

        (b1, p1, b2, p2)
      }
      ._4

  assert(
    maxProfit(Array(1, 2, 4, 2, 5, 7, 2, 4, 9, 0)) == 13,
    s"${maxProfit(Array(1, 2, 4, 2, 5, 7, 2, 4, 9, 0))} != 13"
  )
  assert(maxProfit(Array(3, 3, 5, 0, 0, 2, 1, 4)) == 6, s"${maxProfit(Array(3, 3, 5, 0, 0, 2, 1, 4))} != 6")
  assert(maxProfit(Array(3, 3, 5, 0, 0, 3, 1, 4)) == 6, s"${maxProfit(Array(3, 3, 5, 0, 0, 3, 1, 4))} != 6")

  assert(maxProfit(Array(3, 2, 6, 5, 0, 3)) == 7, s"${maxProfit(Array(3, 2, 6, 5, 0, 3))} != 7")
  assert(
    maxProfit(Array(1, 3, 5, 4, 3, 7, 6, 9, 2, 4)) == 10,
    s"${maxProfit(Array(1, 3, 5, 4, 3, 7, 6, 9, 2, 4))} != 10"
  )
}

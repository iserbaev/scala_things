package leetcode.top150

object MaxProfit extends App {
  def maxProfit(prices: Array[Int]): Int = if (prices.length > 1) {
    val profitFL = prices.tail.foldLeft((prices.head, 0)){ case ((min, profit), e) =>
      (math.min(min, e), math.max(profit, e - min))
    }




    profitFL._2
  } else 0


  assert(maxProfit(Array(7,1,5,3,6,4)) == 5)

  assert(maxProfit(Array(7,6,4,3,1)) == 0)
  assert(maxProfit(Array(2, 1)) == 0)
  assert(maxProfit(Array(2,1,2,0,1)) == 1)
}

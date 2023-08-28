package leetcode.top150

object MaxProfit2 extends App {
  def maxProfit(prices: Array[Int]): Int = if (prices.length > 1) {
    val (_,_,profitSum, profit) = prices.tail.foldLeft((prices.head, prices.head, 0, 0)){ case ((min, max, profitSum, profit), e) =>
      if (max <= e){
        (min, e, profitSum, e - min)
      } else if (min <= e && e <= max){
        (e, e, profitSum + profit, 0)
      } else {
        (e, e, profitSum + profit, 0)
      }
    }

    profitSum + profit
  } else 0

  assert(maxProfit(Array(7,1,5,3,6,4)) == 7)
  assert(maxProfit(Array(7,6,4,3,1)) == 0)
  assert(maxProfit(Array(1,2,3,4,5)) == 4)
}
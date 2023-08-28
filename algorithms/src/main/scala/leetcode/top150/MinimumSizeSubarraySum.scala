package leetcode.top150

object MinimumSizeSubarraySum extends App {
  def minSubArrayLen(target: Int, nums: Array[Int]): Int = {
    if (nums.exists(_ >= target)) {
      1
    } else {
      var startIdx = 0
      var endIdx   = 0
      var len      = Int.MaxValue
      var sum      = 0

      while (endIdx < nums.length) {
        sum += nums(endIdx)

        if (sum >= target) {
          while (sum - nums(startIdx) >= target) {
            sum -= nums(startIdx)
            startIdx += 1
          }
          len = math.min(len, endIdx - startIdx + 1)
        }
        endIdx += 1
      }

      println(len)

      if (len == Int.MaxValue) 0 else len
    }
  }

//  minSubArrayLen(7, Array(2, 3, 1, 2, 4, 3))
  minSubArrayLen(15, Array(1, 2, 3, 4, 5))
}

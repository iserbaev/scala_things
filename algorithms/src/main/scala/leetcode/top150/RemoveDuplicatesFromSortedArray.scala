package leetcode.top150

object RemoveDuplicatesFromSortedArray {
  def removeDuplicates(nums: Array[Int]): Int = if (nums.nonEmpty) {
    val result = nums.tail.foldLeft((1, 1, nums.head)) { case ((uniqueNums, indexToWrite, prevUniqueNum), num) =>
      if (num != prevUniqueNum) {
        nums.update(indexToWrite, num)
        (uniqueNums + 1, indexToWrite + 1, num)
      } else {
        (uniqueNums, indexToWrite, prevUniqueNum)
      }
    }

    result._1
  } else 0
}

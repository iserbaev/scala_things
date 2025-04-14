package leetcode.top150

object RotateArray extends App {
  def rotate(nums: Array[Int], k: Int): Unit = if (k >= 1) {
    val shift         = k % nums.length
    val (left, right) = nums.splitAt(nums.length - shift)
    val newArr        = right ++ left
    newArr.copyToArray(nums): Unit
    ()
  }

  rotate(Array(1, 2, 3, 4, 5, 6, 7), 3)
}

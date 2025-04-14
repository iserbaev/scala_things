package leetcode.top150

object RemoveElement extends App {
  def removeElement(nums: Array[Int], `val`: Int): Int = {
    val filtered = nums.filter(_ != `val`)

    filtered.indices.foreach(i => nums(i) = filtered(i))

    println(nums.mkString(","))

    filtered.length
  }

  removeElement(Array(3, 2, 2, 3), 3): Unit
}

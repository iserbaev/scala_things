package leetcode.top150

object RemoveDuplicates2 extends App {
  def removeDuplicates(nums: Array[Int]): Int = {
    val max        = 100000
    var duplicates = 0
    val occurences = scala.collection.mutable.Map.empty[Int, Int]
    nums
      .sortInPlace()
      .mapInPlace { i =>
        occurences.update(i, occurences.getOrElse(i, 0) + 1)
        if (occurences.getOrElse(i, 0) > 2) {
          duplicates += 1
          max
        } else i
      }
      .sortInPlace()

    println(nums.mkString(", "))
    println(nums.length - duplicates)

    nums.length - duplicates
  }

  removeDuplicates(Array(1, 1, 1, 2, 2, 3))

  removeDuplicates(Array(0, 0, 1, 1, 1, 1, 2, 3, 3))
}

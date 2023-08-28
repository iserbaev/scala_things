package leetcode.top150

object MergeSortedArrays extends App {
  def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = if (m > 0 && n > 0) {
    nums2.indices.foreach(i2 => nums1.update(i2 + m, nums2(i2)))

    java.util.Arrays.sort(nums1)

  } else if (n > 0) {
    nums1.indices.foreach(i => nums1.update(i, nums2(i)))
  }

  merge(Array(1, 2, 3, 0, 0, 0), 3, Array(2, 5, 6), 3)
  merge(Array(4, 5, 6, 0, 0, 0), 3, Array(1, 2, 3), 3)
  merge(Array(4, 0, 0, 0, 0, 0), 1, Array(1, 2, 3, 5, 6), 5)
  merge(Array(1, 2, 4, 5, 6, 0), 5, Array(3), 1)
}

package leetcode.top150

object MajorityElement {
  def majorityElement(nums: Array[Int]): Int =
    nums
      .foldLeft(Map.empty[Int, Int]) { case (acc, i) =>
        acc.updatedWith(i)(_.map(_ + 1).orElse(Some(1)))
      }
      .maxBy(_._2)
      ._1
}

import scala.collection.mutable.ListBuffer

/**
 * There is an array of numbers for example [4,8,8,3,2] that you want to sort in array(s) so that each array that exists consists of no numbers which are more than 2 apart from each other. So in this case we would get two arrays [2,3,4] and [8,8], this would be what your function would return.
 *
 * A couple more examples to try and clarify: [13, 6, 4, 5] would result in [4,5,6] and [13] [22, 23, 17, 18, 11] would result in [11] and [17,18] and [22, 23] [3, 2, 1, 4] would result in [1, 2, 3, 4]
 *
 * Please let me know if anything is unclear or if you have any questions. Please also do not spend too much time one this, the goal is really 30 min.
*/

val arr = Array(4,8,8,3,2)
val arr2 = Array(13, 6, 4, 5)
val arr3 = Array(22, 23, 17, 18, 11)

def sort2(arr: Array[Int]): List[List[Int]] = {
  val res: (ListBuffer[ListBuffer[Int]], ListBuffer[Int]) = arr.sorted.foldLeft((ListBuffer.empty[ListBuffer[Int]],ListBuffer.empty[Int])){
    case ((acc, current),i) =>
      current.lastOption match {
        case Some(value) if i - value <= 2 =>
          current.append(i)
          acc -> current
        case Some(_) =>
          acc.append(current)
           acc -> ListBuffer(i)
        case None =>
          current.append(i)
          acc -> current
      }
  }
  res._1.append(res._2)
  res._1.toList.map(_.toList)
}


assert(sort2(arr) == List(List(2, 3, 4), List(8, 8)))
assert(sort2(arr2) == List(List(4, 5, 6), List(13)))
assert(sort2(arr3) == List(List(11), List(17, 18), List(22, 23)))
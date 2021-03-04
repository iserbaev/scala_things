object RadixSort {
  def sort(a: Array[Int]): Array[Int] = { // Loop for every bit in the integers
    var arr = a
    for (shift <- Integer.SIZE - 1 until -1 by -1) { // The array to put the partially sorted array into
      val tmp = new Array[Int](arr.length)
      // The number of 0s
      var j = 0
      // Move the 0s to the new array, and the 1s to the old one
      for (i <- arr.indices) // If there is a 1 in the bit we are testing, the number will be negative
      // If this is the last bit, negative numbers are actually lower
        if ((shift == 0) == (arr(i) << shift >= 0)) arr(i - j) = arr(i)
        else {
          tmp(j) = arr(i)
          j += 1
        }
      // Copy over the 1s from the old array
      arr.copyToArray(tmp, j, arr.length - j)

      // And now the tmp array gets switched for another round of sorting
      arr = tmp
    }
    arr

  }
}
val sorted = RadixSort.sort(Array(5,6,2,3,4,7,8,33,22,3))
sorted sameElements Array(2, 3, 3, 4, 5, 6, 7, 8, 22, 33)

object Main {
  // https://stackoverflow.com/questions/10321252/mergesort-in-scala
  type Count = Long
  def sortWithInversion(array: Array[Int]): Long = {
    def countSwaps(a: Array[Int]): Count = {
      def mergeRun(li: Int, lend: Int, rb: Int, ri: Int, rend: Int, di: Int, src: Array[Int], dst: Array[Int], swaps: Count): Count = {
        if (ri >= rend && li >= lend) {
          swaps
        } else if (ri >= rend) {
          dst(di) = src(li)
          mergeRun(li + 1, lend, rb, ri, rend, di + 1, src, dst, ri - rb + swaps)
        } else if (li >= lend) {
          dst(di) = src(ri)
          mergeRun(li, lend, rb, ri + 1, rend, di + 1, src, dst, swaps)
        } else if (src(li) <= src(ri)) {
          dst(di) = src(li)
          mergeRun(li + 1, lend, rb, ri, rend, di + 1, src, dst, ri - rb + swaps)
        } else {
          dst(di) = src(ri)
          mergeRun(li, lend, rb, ri + 1, rend, di + 1, src, dst, swaps)
        }
      }

      val b = new Array[Int](a.length)

      def merge(run: Int, run_len: Int, lb: Int, swaps: Count): Count = {
        if (run_len >= a.length) {
          swaps
        } else if (lb >= a.length) {
          merge(run + 1, run_len * 2, 0, swaps)
        } else {
          val lend = math.min(lb + run_len, a.length)
          val rb = lb + run_len
          val rend = math.min(rb + run_len, a.length)
          val (src, dst) = if (run % 2 == 0) (a, b) else (b, a)
          val inc_swaps = mergeRun(lb, lend, rb, rb, rend, lb, src, dst, 0)
          merge(run, run_len, lb + run_len * 2, inc_swaps + swaps)
        }
      }

      merge(0, 1, 0, 0)
    }
    countSwaps(array)
  }

  def main(args: Array[String]): Unit = {
    val n  = scala.io.StdIn.readInt()
    val array = scala.io.StdIn.readLine().split(" ").map(_.toInt)

    def result = sortWithInversion(array)

    println(if (n < 2) 0 else result)
  }
}

import Main.sortWithInversion
def test(): Unit = {
  assert(sortWithInversion(Array(2,3,9,2,9)) == 2)
  assert(sortWithInversion(Array(7,6,5,4,3,2,1)) == 21)
  assert(sortWithInversion(Array(1,2,3,5,4)) == 1)
  assert(sortWithInversion(Array(10, 8, 6, 2, 4, 5)) == 12)
  assert(sortWithInversion(Array(1, 9, 8, 1, 4, 1)) == 8)
  assert(sortWithInversion(Array(6, 5, 8, 6, 0, 4)) == 10)
  assert(sortWithInversion(Array(6, 2, 3, 7, 5, 8)) == 4)
  assert(sortWithInversion(Array(6, 4, 5, 0, 0, 2)) == 11)
  assert(sortWithInversion(Array(8, 9, 10, 7, 4, 0)) == 12)
  assert(sortWithInversion(Array(10, 9, 3, 8, 3, 10)) == 8)
  assert(sortWithInversion(Array(9, 10, 9, 5, 7, 7)) == 10)
  assert(sortWithInversion(Array(9, 5, 8, 9, 4, 10)) == 6)
  assert(sortWithInversion(Array(5, 7, 0, 2, 2, 0)) == 10)
}

def testTime(n: Int = 100000): Long = {
  val arr = scala.util.Random.shuffle((1 to n).toList).toArray
  val before = System.currentTimeMillis()
  sortWithInversion(arr)
  val after = System.currentTimeMillis()
  val diff = after - before
  println(diff)
  assert(diff < 3000, s"$diff > 3000")
  diff
}
test()
testTime(1000)
testTime(10000)
testTime(20000)
testTime(40000)
testTime(100000)
testTime(200000)
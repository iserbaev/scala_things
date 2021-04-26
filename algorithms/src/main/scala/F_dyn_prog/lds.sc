
object Main {
  type Element = Int
  type Index   = Int
  type Count   = Int
//  def lds(ar: Array[Int]): Array[Int] = ??? // TODO

  def lisBottomUp2(n: Int, array: Array[Element]): (Array[Int], Array[Index]) =
    array.zipWithIndex.foldLeft((Array.fill(n)(1), Array.fill(n)(-1))) {
      case ((d, prev), (ai, index)) =>
        (0 to index).foreach(j => {
          if ((array(j) < ai) && (d(j) + 1 > d(index))) {
            d(index)    = d(j) + 1
            prev(index) = j
          }
        })

        (d, prev)
    }
  def returnAns(d: Array[Int], prev: Array[Index]): List[Index] = {
    val max    = d.max
    val (_, k) = d.zipWithIndex.maxBy(t => t._1 + t._2)
    val (lastK, acc) = (1 until max).foldLeft((k, List.empty[Index])) {
      case ((k, acc), _) => (prev(k), k :: acc)
    }
    lastK :: acc
  }
  def lis(array: Array[Element]): List[Index] = {
    val n = array.length

    val (d, prev) = lisBottomUp2(n, array)
    returnAns(d, prev)
  }
  def main(args: Array[String]): Unit = {
    scala.io.StdIn.readLine().toInt
    val array = scala.io.StdIn.readLine().split(" ").map(_.toInt)

    println(lis(array))
  }
  def test(): String = {
    def testOne(
      t:          Array[Int],
      expected:   Array[Int],
      method:     Array[Int] => List[Int],
      methodName: String
    ) = {
      val res = method(t).toArray
      assert(
        res.sameElements(expected),
        s"$methodName(${t.mkString("[", " ", "]")})=${res
          .mkString("[", ",", "]")} =! ${expected.mkString("Array(", ",", ")")}"
      )
      println(s"Result ${res.map(t).mkString(",")}")
    }

    val array = Array(7, 2, 1, 3, 8, 4, 9, 1, 2, 6, 5, 9, 3, 8, 1)
    val n = array.length

    val (d, prev) =
      lisBottomUp2(n, array)
    assert(
      d.sameElements(Array(1, 1, 1, 2, 3, 3, 4, 1, 2, 4, 4, 5, 3, 5, 1)),
      s"d ! ${d.mkString(",")}"
    )
    assert(
      prev.sameElements(
        Array(-1, -1, -1, 1, 3, 3, 4, -1, 2, 5, 5, 9, 8, 9, -1)
      ),
      s"prev ! ${prev.mkString(",")}"
    )

    testOne(
      array,
      Array(1, 3, 5, 9, 13), // (2,3,4,6,8)
      lis,
      "lis"
    )
//    testOne(Array(5,3,4,4,2),Array(1,3,4,5), lds, "lds")
    "success"
  }
}
Main.test()
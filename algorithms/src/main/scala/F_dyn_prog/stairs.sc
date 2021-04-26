object Main {
  implicit class OptOps(val o: Option[Int]) extends AnyVal {
    def +(other: Option[Int]): Option[Int] =
      Option(o.getOrElse(0) + other.getOrElse(0))
  }
  def resolve(frst: Option[Int], sec: Option[Int], th: Option[Int], fth: Option[Int], prevBroken: Boolean, last: Boolean = false): (Int, Boolean) = {
    val arr = if (last) {
      if (prevBroken) {
        Array(
          (frst + sec + th + fth, frst),
          (frst + sec + fth, frst),
          (frst + th + fth, frst)
        )
      } else {
        Array(
          (frst + sec + th + fth, frst),
          (frst + sec + fth, frst),
          (frst + th + fth, frst),
          (sec + th + fth, Some(0)),
          (sec + fth, Some(0))
        )
      }
    } else {
      if (prevBroken) {
        Array(
          (frst + sec + th + fth, frst),
          (frst + sec + th, frst),
          (frst + sec + fth, frst),
          (frst + th + fth, frst),
          (frst + th, frst)
        )
      } else {
        Array(
          (frst + sec + th + fth, frst),
          (frst + sec + th, frst),
          (frst + sec + fth, frst),
          (frst + th + fth, frst),
          (frst + th, frst),
          (sec + th + fth, Some(0)),
          (sec + th, Some(0)),
          (sec + fth, Some(0))
        )
      }
    }

    val max = arr.maxBy(_._1)
    val res = if (last) max._1 else max._2
    (res.get, max._2 != frst)
  }
  def stairs(arr: Array[Int]): Int = {
    val withIndex = arr.zipWithIndex
    val lastIndex = withIndex.lastOption.map(_._2).getOrElse(0)
    val accSum = List.empty[Int]

    val (acc,_,(_,_,_)) = withIndex.foldLeft((accSum, false, (Option.empty[Int],Option.empty[Int],Option.empty[Int]))){
      case ((acc, prevBroken, (ppp,pp,p)), (el,index)) =>
        (ppp, pp, p) match {
          case _ if index == lastIndex =>
            val (included, newPrevBroken) = resolve(ppp, pp, p, Some(el), prevBroken, last = index == lastIndex)
            (acc :+ included, newPrevBroken, (pp,p, Some(el)))
          case (Some(_), Some(_),Some(_)) =>
            val (included, newPrevBroken) = resolve(ppp, pp, p, Some(el), prevBroken, last = index == lastIndex)
            (acc :+ included, newPrevBroken, (pp,p, Some(el)))
          case _ =>
            (acc, prevBroken, (pp,p, Some(el)))
        }
    }

    acc.sum
  }

  def main(args: Array[String]): Unit = {
    scala.io.StdIn.readLine().toInt
    val s2 = scala.io.StdIn.readLine().split(" ").map(_.toInt)

    println(stairs(s2))
  }

  def test() = {
    assert(-63 == stairs(Array(-2, -16, -13, -9, -48)),1)
    assert(2 == stairs(Array(1, 1, -2, -4, -6, 2, 2)),2)
    assert(-73 == stairs(Array(-64, -16, -13, -9, -48)),3)
    assert(5 == stairs(Array(0, 0, 0, 4, 6, -5)),4)
    assert(-9 == stairs(Array(-6, 4, -16, -13, -9, 0)),5)
    assert(-18 == stairs(Array(-6, 4, -16, -13, -9)),6)
    assert(21 == stairs(Array(3, 4, 10, 10, 0, -6, -10, 0)),7)
    assert(3 == stairs(Array(1, 2)),8)
    assert(1 == stairs(Array(2, -1)),9)
    assert(3 == stairs(Array(-1, 2, 1)),10)
    assert(2 == stairs(Array(2)),11)
    assert(-2 == stairs(Array(-2)),12)
    assert(29 == stairs(Array(-5,8, 10, 7, -2, 4)), 13)
  }
}
Main.test()
package tasks

/** 1) Cжать последовательность интов Seq(1, 2, 2, 3, 4, 3, 3, 3) => Seq((1, 1), (2, 2),
  * (3, 1), (4, 1), (3, 3)) Ответ выдать в виде Seq[(Int, Int)] (число из
  * последовательности и число последовательных повторений) 2) восстановаить исходную
  * последовательность из сжатой
  */
object ZipSeq extends App {

  val in = Seq(1, 2, 2, 3, 4, 3, 3, 3)

  val ((last, count), acc) =
    in.tail.foldLeft(((in.head, 1), Seq.empty[(Int, Int)])) { case (((prev, count), acc), el) =>
      if (el == prev) {
        ((prev, count + 1), acc)
      } else {
        ((el, 1), acc.:+((prev, count)))
      }
    }

  val result = acc.:+((last, count))

  println(result.mkString("\n"))

  val r2 = result.flatMap { case (el, count) =>
    (1 to count).map(_ => el).toSeq
  }

  println(r2)
}

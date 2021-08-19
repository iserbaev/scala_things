package tasks

import scala.io.Source

object Inf3Tasks extends App {
  // Нажав на имени, скачайте файл
  //
  //max_min_14.zip
  //
  //с не более чем 5000000 целых значений.
  //
  //Найдите максимальное max и минимальное min значения.
  def getMinMax() = {
    val mmfile = "max_min_9.txt"

    val bs: Seq[Int] = getResource(mmfile).map(_.toInt)

    val minMaxes = bs
    val min      = minMaxes.min
    val max      = minMaxes.max

    println(max)
    println(min)
  }

  private def getResource(uri: String) = {
    val stream = this.getClass.getClassLoader.getResourceAsStream(uri)
    val seq    = Source.fromInputStream(stream).getLines().toSeq
    seq
  }

  //Нажав на имени, скачайте файл
  //
  //sum_recurr_rd_2.zip
  //
  //с не более чем 1000 целых значений, представляющих члены ряда a(k), начиная с k=0.
  //
  //Вычислите значение S(250), начиная с k=17, если
  //
  //S(k)=S(k-1)+a(k+1)+a(k) и S(16)=7.
  //
  def sumRecurr() = {
    @scala.annotation.tailrec
    def recur(sum: Long, source: List[Int]): Long = source match {
      case Nil                  => sum
      case one :: Nil           => recur(sum + one, Nil)
      case head :: next :: tail => recur(sum + next + head, tail)
    }

    val srfile = "sum_recurr_rd_15.txt"

    val bs = getResource(srfile).map(_.toInt).toList

    val heads = bs.take(301)
    val slice = bs.slice(301, 430)

    println(slice.size)

    println(recur(0, heads))
    println(recur(-909, slice))

  }

  import scala.math._
  def summ() = {
    def fun(i: Int): Int = {
      val f    = pow(-1d, i.toDouble - 1)
      val sec  = pow(9d / 7, i.toDouble - 1)
      val thrd = pow(4d / 5, i.toDouble)
      val res  = f * (sec + thrd)
      res.toInt
    }
    val sum = (10 to 60).toList.map(fun).sum

    println(sum)
  }

  def sort() = {
    val file = "sort7.txt"
    val seq  = getResource(file).map(_.toLong).toList

    println(seq.min)
    println(seq.sorted(Ordering.Long.reverse).take(5).mkString(";"))
  }

  getMinMax()
  sumRecurr()
  summ()
  sort()
}

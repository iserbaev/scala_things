package tasks

import scala.collection.immutable.SortedMap

/** Даны три массива целых положительных чисел A, B, C, а также целое число S.
  *
  * Найдите такие i, j, k, что A i + B j + C k = S
  *
  * Если подходящих троек несколько, выведите ту, для которой (i,j,k) лексикографически
  * минимально. Иными словами, среди подходящих троек выберите ту, у которой минимально i,
  * среди таких — ту, у которой минимально j, среди оставшихся — ту, у которой минимально
  * k.
  *
  * Первая строка содержит одно целое число S (1≤S≤ 1 0 9 10 9 ).
  *
  * Каждая их трёх следующих строк содержит описание одного из массивов A, B, C в
  * следующем формате. Первое число задаёт длину n (1≤n≤15000) соответствующего массива, а
  * следующие n чисел задают сам массив. Элементы каждого массива — целые числа от 1 до 1
  * 0 9 10 9 .
  */
object StepicTripleMain {
  case class TripleStat(idxSum: Int, i: Int, j: Int, k: Int)
  val emptyTripleStat = TripleStat(-1, -1, -1, -1)

  def arrayHeadTail[T](array: Array[T]): (T, Array[T]) = {
    require(array.nonEmpty, "Array is empty")

    (array.head, array.tail)
  }

  def main(args: Array[String]): Unit = {
    val s            = scala.io.StdIn.readInt()
    val (n1, arrayI) = arrayHeadTail(scala.io.StdIn.readLine().split(" ").map(_.toLong))
    val (n2, arrayJ) = arrayHeadTail(scala.io.StdIn.readLine().split(" ").map(_.toLong))
    val (n3, arrayK) = arrayHeadTail(scala.io.StdIn.readLine().split(" ").map(_.toLong))

    if (n1 > 0 && n2 > 0 && n3 > 0) {
      // O(n log n)
      val sortedMapJ = SortedMap.newBuilder[Long, Int].++=(arrayJ.zipWithIndex.reverse).result()
      // O(n log n)
      val sortedMapK = SortedMap.newBuilder[Long, Int].++=(arrayK.zipWithIndex.reverse).result()

      // O(i * j)
      val result = arrayI.zipWithIndex.foldLeft(emptyTripleStat) { case (currentTripleStat, (iValue, iIdx)) =>
        // O(j)
        val jTuples = sortedMapJ.filter(_._1 < s - iValue)

        def checkSortedMapJ(ts: TripleStat, jValue: Long, jIdx: Int): TripleStat =
          if (jValue + iValue >= s) ts
          else {
            // O (log k)
            val kValue = s - jValue - iValue
            val kIdx = sortedMapK.get(kValue)
            kIdx match {
              case Some(kIdx) if ts.idxSum == -1 =>
                TripleStat(iIdx + jIdx + kIdx, iIdx, jIdx, kIdx)
              case Some(kIdx) if ((iIdx + jIdx + kIdx) == ts.idxSum) && jIdx < ts.j =>
                TripleStat(iIdx + jIdx + kIdx, iIdx, jIdx, kIdx)
              case Some(kIdx) if (iIdx + jIdx + kIdx) < ts.idxSum =>
                TripleStat(iIdx + jIdx + kIdx, iIdx, jIdx, kIdx)
              case _ =>
                ts
            }
          }

        // O(j)
        jTuples
          .foldLeft(currentTripleStat) { case (ts, (je, idx)) =>
            checkSortedMapJ(ts, je, idx)
          }
      }

      if (result.idxSum > 0) println(s"${result.i} ${result.j} ${result.k}") else println(-1)
    } else {
      println(-1)
    }
  }
}

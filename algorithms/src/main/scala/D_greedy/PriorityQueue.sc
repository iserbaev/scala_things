/**
  * Первая строка входа содержит число операций
  * 1≤n≤10x5. Каждая из последующих
  * n строк задают операцию одного из следующих двух типов:
  * Insert x, где 0≤x≤10*9 — целое число;
  * ExtractMax.
  * Первая операция добавляет число
  * x в очередь с приоритетами, вторая — извлекает максимальное число и выводит его.
  * Sample Input:
  * 6
  * Insert 200
  * Insert 10
  * ExtractMax
  * Insert 5
  * Insert 500
  * ExtractMax
  * Sample Output:
  * 200
  * 500
  */
object Main {
  import scala.io.StdIn._
  import scala.collection.mutable
  def main(args: Array[String]): Unit = {
    implicit val ord: Ordering[Int] = Ordering.Int
    val queue = mutable.PriorityQueue.empty[Int]
    val n     = readInt()
    (1 to n).foreach(_ => {
      val arr = readLine().split(" ")
      if (arr.size == 1) {
        println(queue.dequeue())
      } else {
        val (_, element) = (arr.head, arr.last.toInt)
        queue.enqueue(element)
      }
    })
  }
}

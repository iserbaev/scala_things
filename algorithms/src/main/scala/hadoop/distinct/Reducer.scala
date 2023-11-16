package hadoop.distinct

import scala.collection.immutable.SortedSet

object Reducer {

  def processLine(sortedSet: SortedSet[String]): SortedSet[String] =
    recur(sortedSet)

  @scala.annotation.tailrec
  private def recur(sortedSet: SortedSet[String]): SortedSet[String] = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) {
      return sortedSet
    }
    val data = line.split("\t")

    recur(sortedSet + data.head)
  }

  def main(args: Array[String]): Unit = {
    val sortedSet = processLine(SortedSet.empty[String])
    sortedSet.foreach(println)
  }
}

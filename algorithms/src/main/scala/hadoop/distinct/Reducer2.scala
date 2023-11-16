package hadoop.distinct

import scala.collection.immutable.ListMap

object Reducer2 {

  @scala.annotation.tailrec
  def processLine(m: ListMap[String, Set[Int]]): ListMap[String, Set[Int]] = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) return m

    val Array(value, groupsRaw) = line.split("\t")
    val groups                  = groupsRaw.split(",").map(_.trim)

    val resultMap = groups.foldLeft(m) { case (acc, g) =>
      acc.updated(g, acc.get(g).map(_ + value.toInt).getOrElse(Set(value.toInt)))
    }

    processLine(resultMap)
  }

  def main(args: Array[String]): Unit =
    processLine(ListMap.empty[String, Set[Int]])
      .foreach { case (k, v) => println(k ++ s"\t${v.size.toString}") }
}

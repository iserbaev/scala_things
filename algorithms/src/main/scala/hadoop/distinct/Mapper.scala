package hadoop.distinct

import scala.collection.immutable.ListMap

object Mapper {

  @scala.annotation.tailrec
  def processLine(): Unit = {
    val m    = ListMap.empty[String, Int]
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) return ()
    val Array(value, groupsRaw) = line.split("\t")
    val groups                  = groupsRaw.split(",").map(_.trim)

    val resultMap = groups.foldLeft(m) { case (acc, g) =>
      val key = s"$value,$g"
      acc.updated(key, acc.get(key).map(_ + 1).getOrElse(1))
    }

    resultMap.foreach { case (k, v) => println(k ++ s"\t${v.toString}") }

    processLine()
  }

  def main(args: Array[String]): Unit =
    processLine()
}

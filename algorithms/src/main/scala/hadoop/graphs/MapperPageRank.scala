package hadoop.graphs

import java.math.MathContext

object MapperPageRank {

  @scala.annotation.tailrec
  def processLine(): Unit = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) return ()
    println(line) // forward to reducer for reconstructing graph
    val List(_, pageRankRaw, adjacentsRaw) = line.split("\t", 3).toList

    val pageRank = BigDecimal.apply(pageRankRaw.trim, new MathContext(3))

    val adjacents = adjacentsRaw
      .replaceAll("\\{", "")
      .replaceAll("}", "")
      .split(",")
      .filter(_.nonEmpty)

    val pageRankForEachAdjacent = pageRank / adjacents.length


      adjacents.foreach { a =>
        if (a.trim.nonEmpty) println(a ++ "\t" ++ pageRankForEachAdjacent.rounded.toString ++ "\t" ++ "{}")
      }

    processLine()
  }

  def main(args: Array[String]): Unit =
    processLine()
}

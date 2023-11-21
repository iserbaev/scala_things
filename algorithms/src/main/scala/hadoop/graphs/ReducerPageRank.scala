package hadoop.graphs

import java.math.MathContext
import scala.collection.immutable.ListMap

object ReducerPageRank {

  @scala.annotation.tailrec
  def processLine(m: ListMap[String, (BigDecimal, String)]): ListMap[String, (BigDecimal, String)] = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) return m
    val List(v, pageRankRaw, adjacentsRaw) = line.split("\t", 3).toList

    val pageRank = BigDecimal.apply(pageRankRaw.trim, new MathContext(3))

    val updatedAcc = m.get(v) match {
      case Some((prOld, aOld)) =>
        if (isNode(adjacentsRaw)) m.updated(v, (prOld, adjacentsRaw))
        else {
          m.updated(v, (pageRank + prOld, aOld))
        }
      case None =>
        if (isNode(adjacentsRaw)) m.updated(v, (BigDecimal(0), adjacentsRaw))
        else {
          m.updated(v, (pageRank, adjacentsRaw))
        }
    }

    processLine(updatedAcc)
  }

  def isEmptyAdjacents(a: String): Boolean = a.contains("{}")

  def isNode(a: String): Boolean = !isEmptyAdjacents(a)

  def main(args: Array[String]): Unit =
    processLine(ListMap.empty[String, (BigDecimal, String)]).foreach { case (v, (d, a)) =>
      println(v ++ "\t" ++ d.toString() ++ "\t" ++ a)
    }
}

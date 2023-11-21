package hadoop.graphs

import scala.collection.immutable.ListMap

object ReducerBFS {

  @scala.annotation.tailrec
  def processLine(m: ListMap[String, (String, String)]): ListMap[String, (String, String)] = {

    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) return m
    val List(v, distance, adjacentsRaw) = line.split("\t", 3).toList
    val updatedAcc = m.get(v) match {
      case Some((dOld, aOld)) =>
        if (isNode(adjacentsRaw)) m.updated(v, (dmin(dOld, distance), adjacentsRaw))
        else {
          m.updated(v, (dmin(dOld, distance), aOld))
        }
      case None =>
        m.updated(v, (distance, adjacentsRaw))
    }

    processLine(updatedAcc)
  }

  def dmin(d1: String, d2: String) = (isINF(d1), isINF(d2)) match {
    case (false, false) => math.min(d1.trim.toInt, d2.trim.toInt).toString
    case (false, _)     => d1
    case _              => d2
  }
  def isINF(d: String): Boolean            = d.contains("INF")
  def isEmptyAdjacents(a: String): Boolean = a.contains("{}")

  def isNode(a: String): Boolean = !isEmptyAdjacents(a)

  def main(args: Array[String]): Unit =
    processLine(ListMap.empty[String, (String, String)]).foreach { case (v, (d, a)) =>
      println(v ++ "\t" ++ d ++ "\t" ++ a)
    }
}

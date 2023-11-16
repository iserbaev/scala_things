package hadoop.tfidf

import scala.collection.immutable.ListMap

object ReducerTF {

  @scala.annotation.tailrec
  def processLine(m: ListMap[String, Int]): ListMap[String, Int] = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) return m

    val Array(key, count) = line.split("\t", 2)

    processLine(m.updated(key, m.get(key).map(_ + count.toInt).getOrElse(count.toInt)))
  }

  def main(args: Array[String]): Unit =
    processLine(ListMap.empty[String, Int])
      .foreach { case (k, v) =>
        val Array(word, docNr) = k.split("#", 2)
        println(word ++ "\t" ++ docNr ++ "\t" ++ v.toString)
      }
}

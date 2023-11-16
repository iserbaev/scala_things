package hadoop.tfidf

import scala.collection.immutable.ListMap

object ReducerTFIDF {

  @scala.annotation.tailrec
  def processLine(m: ListMap[String, IndexedSeq[String]]): ListMap[String, IndexedSeq[String]] = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) return m

    val Array(word, triple) = line.split("\t", 2)

    processLine(m.updated(word, m.get(word).map(_.:+(triple)).getOrElse(IndexedSeq(triple))))
  }

  def main(args: Array[String]): Unit =
    processLine(ListMap.empty[String, IndexedSeq[String]])
      .foreach { case (k, v) =>
        val docCnt = v.length
        v.foreach { triple =>
          val Array(docNr, tf, _) = triple.split(";", 3)

          println(s"$k#$docNr\t$tf\t$docCnt")
        }
      }
}

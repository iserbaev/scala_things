package hadoop.tfidf

object MapperTFIDF {

  @scala.annotation.tailrec
  def processLine(): Unit = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) return ()
    val Array(word, docNr, count) = line.split("\t", 3)
    println(s"$word\t$docNr;$count;1")

    processLine()
  }

  def main(args: Array[String]): Unit =
    processLine()
}

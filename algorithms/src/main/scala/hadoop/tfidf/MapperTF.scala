package hadoop.tfidf

object MapperTF {

  @scala.annotation.tailrec
  def processLine(): Unit = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) return ()
    val Array(docNr, text) = line.split(":", 2)
    val words              = text.replaceAll("[^a-zA-Z0-9]", " ").split(" ")
    words.foreach { w =>
      if (w.nonEmpty) println(s"$w#$docNr\t1")
    }

    processLine()
  }

  def main(args: Array[String]): Unit =
    processLine()
}

package hadoop.relations

object MapperFilter {

  @scala.annotation.tailrec
  def processLine(userName: String): Unit = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) return ()
    val Array(_,user,_) = line.split("\t")

    if (user.trim()  == userName) println(line)

    processLine(userName)
  }

  def main(args: Array[String]): Unit =
    processLine("user10")
}

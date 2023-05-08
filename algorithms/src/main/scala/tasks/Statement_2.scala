package tasks

object Statement_2 {

  case class Packet(i: Int, arrival: Long, duration: Long)
  def process(
      bufferSize: Int,
      packets: IndexedSeq[Packet]
  ): IndexedSeq[Long] =
    packets
      .foldLeft((IndexedSeq(0L), IndexedSeq.empty[Long])) { case ((buffer, logs), packet) =>
        if (packet.arrival < buffer.head) {
          if (buffer.length < bufferSize) {
            val startTime = buffer.lastOption.getOrElse(buffer.head)
            buffer.:+(startTime + packet.duration) -> logs.:+(startTime)
          } else {
            buffer -> logs.:+(-1L)
          }

        } else {
          val filtered  = buffer.filter(_ > packet.arrival)
          val startTime = filtered.lastOption.getOrElse(packet.arrival)

          filtered.:+(startTime + packet.duration) -> logs.:+(
            startTime
          )
        }
      }
      ._2
}

object TestApp extends App {
  def test(in: String, expected: String): Unit = {
    val lines = in.split("\n")
    val (bufferSize, _) = {
      val h = lines.head.split(" ")
      (h.head.toInt, h.last.toInt)
    }
    val arrivalWithDurations = lines.tail.map { s =>
      val tmp = s.split(" ")
      tmp.head.toLong -> tmp.last.toLong
    }

    val packets = arrivalWithDurations.zipWithIndex.map(t => Statement_2.Packet(t._2, t._1._1, t._1._2)).toIndexedSeq

    val result = Statement_2.process(bufferSize, packets).mkString(" ")
    println(s"""result   = ($result) ${if (result != expected) "!!!" else ""}
               |expected = ($expected) """.stripMargin)
  }

  val t1 =
    s"""2 8
       |0 0
       |0 0
       |0 0
       |1 0
       |1 0
       |1 1
       |1 2
       |1 3""".stripMargin
  val t1Expected = "0 0 0 1 1 1 2 -1"
  test(t1, t1Expected)

  val t2 =
    s"""2 8
       |0 0
       |0 0
       |0 0
       |1 1
       |1 0
       |1 0
       |1 2
       |1 3""".stripMargin
  val t2expected = "0 0 0 1 2 -1 -1 -1"
  test(t2, t2expected)

  val t3 =
    s"""1 5
       |999999 1
       |1000000 0
       |1000000 1
       |1000000 0
       |1000000 0""".stripMargin
  val t3expected = "999999 1000000 1000000 -1 -1"
  test(t3, t3expected)

  val t4 =
    s"""3 6
       |0 7
       |0 0
       |2 0
       |3 3
       |4 0
       |5 0""".stripMargin
  val t4expected = "0 7 7 -1 -1 -1"
  test(t4, t4expected)

  val t5 =
    s"""2 6
       |0 2
       |0 0
       |2 0
       |3 0
       |4 0
       |5 0""".stripMargin
  val t5expected = "0 2 2 3 4 5"
  test(t5, t5expected)

  val t6 =
    s"""2 5
       |2 9
       |4 8
       |10 9
       |15 2
       |19 1""".stripMargin
  val t6expected = "2 11 -1 19 21"
  test(t6, t6expected)
}

package tasks

object Statement_3 {

  case class Packet(arrival: Long, duration: Long)
  sealed trait State {
    def arrival: Long
    def packets: IndexedSeq[Packet]
    def processingLogs: IndexedSeq[Long]
    def notProcessed: Int
    def logs: String = {
      val resultLogs = processingLogs ++ Seq.fill[Long](notProcessed)(-1)
      if (resultLogs.nonEmpty) {
        resultLogs.mkString(" ")
      } else {
        ""
      }
    }
  }
  object State {
    case class Worked(
        running: Packet,
        packets: IndexedSeq[Packet],
        processingLogs: IndexedSeq[Long],
        notProcessed: Int
    ) extends State {
      val arrival: Long   = running.arrival
      def bufferSize: Int = packets.length + 1

      def addPacket(next: Packet): State =
        copy(packets = packets.:+(next))

      def skipPacket: State =
        copy(notProcessed = notProcessed + 1)

      def up(next: Packet): State = {
        val quant = next.arrival - arrival
        if (quant >= running.duration) {
          if (packets.nonEmpty) {
            val toRun = packets.head.copy(
              arrival = packets.head.arrival + running.duration
            )
            Worked(
              toRun,
              packets.tail,
              processingLogs.:+(toRun.arrival),
              notProcessed
            ).up(next)
          } else {
            Vacant(next.arrival, processingLogs, notProcessed)
          }
        } else {
          val remaining = running
            .copy(arrival = next.arrival, duration = running.duration - quant)
          val updatedPackets =
            packets.map(_.copy(arrival = next.arrival))
          Worked(remaining, updatedPackets, processingLogs, notProcessed)
        }
      }
      def finish(): State =
        if (packets.isEmpty) {
          Vacant(arrival + running.duration, processingLogs, notProcessed)
        } else {
          val updated =
            packets.map(_.copy(arrival = arrival + running.duration))
          Worked(
            updated.head,
            updated.tail,
            processingLogs.:+(updated.head.arrival),
            notProcessed
          ).finish()
        }
    }

    case class Vacant(
        arrival: Long,
        processingLogs: IndexedSeq[Long],
        notProcessed: Int
    ) extends State {
      override def packets: IndexedSeq[Packet] = IndexedSeq.empty
    }

    @scala.annotation.tailrec
    def process(bufferSize: Int, state: State, next: Packet): State =
      state match {
        case v: Vacant =>
          if (next.duration == 0)
            Vacant(
              next.arrival,
              v.processingLogs.:+(next.arrival),
              v.notProcessed
            )
          else
            Worked(
              next,
              IndexedSeq(),
              v.processingLogs.:+(next.arrival),
              v.notProcessed
            )
        case w: Worked if next.arrival == w.arrival =>
          if (w.running.duration == 0) {
            Worked(
              next,
              w.packets,
              w.processingLogs,
              w.notProcessed
            )
          } else {
            if (w.bufferSize < bufferSize)
              w.addPacket(next)
            else
              w.skipPacket
          }
        case w: Worked if next.arrival != w.arrival =>
          process(bufferSize, w.up(next), next)
        case _ =>
          throw new IllegalStateException("should not happen")
      }
  }

  import State._
  def process(
      bufferSize: Int,
      packetsCount: Int,
      arrivalWithDuration: Seq[(Long, Long)]
  ): State = {
    val result =
      if (arrivalWithDuration.nonEmpty)
        (0 until packetsCount).foldLeft(Vacant(0, IndexedSeq.empty, 0): State) { case (state, i) =>
          val (arrival, duration) = arrivalWithDuration(i)
          State.process(bufferSize, state, Packet(arrival, duration))
        }
      else {
        Vacant(0, IndexedSeq.empty, 0)
      }

    result match {
      case w: Worked =>
        w.finish()
      case v: Vacant =>
        v
    }
  }
}

object TestApp2 extends App {
  def test(in: String, expected: String): Unit = {
    val lines = in.split("\n")
    val (bufferSize, n) = {
      val h = lines.head.split(" ")
      (h.head.toInt, h.last.toInt)
    }
    val arrivalWithDurations = lines.tail.map { s =>
      val tmp = s.split(" ")
      tmp.head.toLong -> tmp.last.toLong
    }.toSeq

    val result = Statement_3.process(bufferSize, n, arrivalWithDurations)
    println(result.logs)
    assert(result.logs == expected)
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
}

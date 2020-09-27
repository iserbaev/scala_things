package tasks

object Statement_2 {

  case class Packet(i: Int, arrival: Long, duration: Long)
  sealed trait State {
    def arrival:        Long
    def packets:        IndexedSeq[Packet]
    def processingLogs: IndexedSeq[Long]
    def logs: String =
      if (processingLogs.nonEmpty) {
        processingLogs.mkString(" ")
      } else {
        ""
      }
  }
  object State {
    def vacant(packetsCount: Int): State =
      Vacant(0, IndexedSeq.fill[Long](packetsCount)(-1))
    def run(logs: IndexedSeq[Long], packet: Packet): State =
      if (packet.duration == 0)
        Vacant(
          packet.arrival,
          logs.updated(packet.i, packet.arrival)
        )
      else
        Worked(
          packet,
          IndexedSeq(),
          logs.updated(packet.i, packet.arrival)
        )
    case class Worked(
      running:        Packet,
      packets:        IndexedSeq[Packet],
      processingLogs: IndexedSeq[Long]
    ) extends State {
      val arrival:    Long = running.arrival
      def bufferSize: Int  = packets.length + 1

      def addPacket(next: Packet): State =
        copy(packets = packets.:+(next))

      def skipPacket(next: Packet): State =
        copy(processingLogs = processingLogs.updated(next.i, -1L))

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
              processingLogs.updated(toRun.i, toRun.arrival)
            ).up(next)
          } else {
            Vacant(next.arrival, processingLogs)
          }
        } else {
          val remaining = running
            .copy(arrival = next.arrival, duration = running.duration - quant)
          val updatedPackets =
            packets.map(_.copy(arrival = next.arrival))
          Worked(remaining, updatedPackets, processingLogs)
        }
      }
      def finish(): State =
        if (packets.isEmpty) {
          Vacant(arrival + running.duration, processingLogs)
        } else {
          val updated =
            packets.map(_.copy(arrival = arrival + running.duration))
          Worked(
            updated.head,
            updated.tail,
            processingLogs.updated(updated.head.i, updated.head.arrival)
          ).finish()
        }
    }

    case class Vacant(
      arrival:        Long,
      processingLogs: IndexedSeq[Long]
    ) extends State {
      override def packets: IndexedSeq[Packet] = IndexedSeq.empty
    }

    @scala.annotation.tailrec
    def process(bufferSize: Int, state: State, next: Packet): State =
      state match {
        case v: Vacant =>
          State.run(v.processingLogs, next)
        case w: Worked if next.arrival == w.arrival =>
          if (w.running.duration == 0) {
            w.up(next)
          } else {
            if (w.bufferSize < bufferSize)
              w.addPacket(next)
            else
              w.skipPacket(next)
          }
        case w: Worked if next.arrival != w.arrival =>
          val up = w.up(next)
          process(bufferSize, up, next)
      }
  }

  import State._
  def process(
    bufferSize:   Int,
    packetsCount: Int,
    packets:      IndexedSeq[Packet]
  ): State = {
    val result =
      if (packets.nonEmpty)
        packets.foldLeft(State.vacant(packetsCount)) {
          case (state, packet) =>
            State.process(bufferSize, state, packet)
        } else {
        State.vacant(packetsCount)
      }

    result match {
      case w: Worked =>
        w.finish()
      case v: Vacant =>
        v
    }
  }
}

object TestApp extends App {
  def test(in: String, expected: String): Unit = {
    val lines = in.split("\n")
    val (bufferSize, n) = {
      val h = lines.head.split(" ")
      (h.head.toInt, h.last.toInt)
    }
    val arrivalWithDurations = lines.tail.map(s => {
      val tmp = s.split(" ")
      tmp.head.toLong -> tmp.last.toLong
    })

    val packets = arrivalWithDurations.zipWithIndex.map(
      t => Statement_2.Packet(t._2, t._1._1, t._1._2)
    )

    val result = Statement_2.process(bufferSize, n, packets)
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

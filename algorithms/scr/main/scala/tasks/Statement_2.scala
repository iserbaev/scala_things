package tasks

object Statement_2 {

  case class Packet(arrival: Long, duration: Long)
  sealed trait State {
    def arrival:        Long
    def durations:      IndexedSeq[Long]
    def processingLogs: IndexedSeq[Long]
    def logs: String =
      if (processingLogs.nonEmpty) {
        processingLogs.mkString(" ")
      } else {
        ""
      }
  }
  object State {
    case class Worked(
      arrival:        Long,
      durations:      IndexedSeq[Long],
      processingLogs: IndexedSeq[Long],
      notProcessed:   Int
    ) extends State {
      def addPacket(next: Packet): State =
        copy(durations = durations.:+(next.duration))

      def skipPacket: State =
        copy(notProcessed = notProcessed + 1)

      def run(nextArrival: Long): State = {
        val (_, logs, acc) =
          durations.foldLeft((arrival, processingLogs, IndexedSeq.empty[Long])) {
            case ((currentArrival, logs, acc), d) =>
              if (currentArrival <= nextArrival) {
                (currentArrival + d, logs.:+(currentArrival), acc)
              } else {
                (
                  currentArrival + d,
                  logs,
                  acc.:+(currentArrival + d)
                )
              }
          }

        val resultLogs = logs ++ Seq.fill[Long](notProcessed)(-1)
        if (acc.isEmpty) Vacant(nextArrival, resultLogs)
        else Worked(nextArrival, acc, resultLogs, 0)
      }
    }
    case class Vacant(arrival: Long, processingLogs: IndexedSeq[Long])
        extends State {
      override def durations: IndexedSeq[Long] = IndexedSeq.empty
    }

    @scala.annotation.tailrec
    def process(bufferSize: Int, state: State, next: Packet): State =
      state match {
        case v: Vacant =>
          if (next.duration == 0)
            Vacant(next.arrival, v.processingLogs.:+(next.arrival))
          else
            Worked(next.arrival, IndexedSeq(next.duration), v.processingLogs, 0)
        case w: Worked if next.arrival == w.arrival =>
          if (w.durations.length < bufferSize)
            w.addPacket(next)
          else
            w.skipPacket
        case w: Worked if next.arrival != w.arrival =>
          process(bufferSize, w.run(next.arrival), next)
      }
  }

  import State._
  def process(
    bufferSize:          Int,
    packetsCount:        Int,
    arrivalWithDuration: IndexedSeq[(Long, Long)]
  ): State = {
    val result =
      if (arrivalWithDuration.nonEmpty)
        (0 until packetsCount).foldLeft(Vacant(0, IndexedSeq.empty): State) {
          case (state, i) =>
            val (arrival, duration) = arrivalWithDuration(i)
            State.process(bufferSize, state, Packet(arrival, duration))
        } else {
        Vacant(0, IndexedSeq.empty)
      }

    result match {
      case w: Worked =>
        w.run(Int.MaxValue)
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

    val result = Statement_2.process(bufferSize, n, arrivalWithDurations)
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

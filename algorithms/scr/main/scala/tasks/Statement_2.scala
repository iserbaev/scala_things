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
        "0"
      }
  }
  object State {
    case class Worked(
      arrival:           Long,
      durations:         IndexedSeq[Long],
      processingLogs:    IndexedSeq[Long],
      notProcessedCount: Int
    ) extends State {
      def run(quant: Long): State = {
        val (logs, acc) =
          durations.foldLeft((processingLogs, IndexedSeq.empty[Long])) {
            case ((logs, acc), d) =>
              if (d <= quant) {
                logs.:+(d) -> acc
              } else {
                logs -> acc.:+(d - quant)
              }
          }

        val resultLogs: IndexedSeq[Long] = logs ++ Seq.fill[Long](
            notProcessedCount
          )(
            -1
          )
        if (acc.isEmpty) Vacant(arrival, resultLogs)
        else Worked(arrival, acc, resultLogs, 0)
      }
    }
    case class Vacant(arrival: Long, processingLogs: IndexedSeq[Long])
        extends State {
      override def durations: IndexedSeq[Long] = IndexedSeq.empty
    }

    def process(bufferSize: Int, state: State, next: Packet): State =
      state match {
        case v: Vacant if next.duration == 0 =>
          Vacant(next.arrival, v.processingLogs.:+(next.arrival))
        case v: Vacant =>
          Worked(next.arrival, IndexedSeq(next.duration), v.processingLogs, 0)
        case w: Worked
            if next.arrival == w.arrival & w.durations.length < bufferSize =>
          w.copy(durations = w.durations.:+(next.duration))
        case w: Worked
            if next.arrival == w.arrival & w.durations.length >= bufferSize =>
          w.copy(notProcessedCount = w.notProcessedCount + 1)
        case w: Worked if next.arrival != w.arrival =>
          val result = w.run(next.arrival - w.arrival)
          if (result.durations.length >= bufferSize) {
            Worked(next.arrival, result.durations, result.processingLogs, 1)
          } else {
            Worked(
              next.arrival,
              result.durations.:+(next.duration),
              result.processingLogs,
              0
            )
          }

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

    val result = Statement_2
      .process(bufferSize, n, arrivalWithDurations)

    println(result)
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

}

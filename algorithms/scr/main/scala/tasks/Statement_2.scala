package tasks

import tasks.Statement_2.State.Worked

object Statement_2 {

  case class Packet(arrival: Long, duration: Long)
  sealed trait State {
    def maxBufferSize:  Int
    def buffer:         IndexedSeq[Packet]
    def processingLogs: IndexedSeq[Long]
    def logs: String =
      if (processingLogs.nonEmpty) {
        processingLogs.mkString(" ")
      } else {
        "0"
      }
    def add(packet: Packet, logs: IndexedSeq[Long]): State =
      if (buffer.length < maxBufferSize) {
        Worked(maxBufferSize, buffer.:+(packet), logs)
      } else {
        Worked(maxBufferSize, buffer, logs.:+(-1L))
      }
  }
  object State {
    case class Worked(
      maxBufferSize:  Int,
      buffer:         IndexedSeq[Packet],
      processingLogs: IndexedSeq[Long]
    ) extends State
    case class Vacant(maxBufferSize: Int, processingLogs: IndexedSeq[Long])
        extends State {
      override def buffer: IndexedSeq[Packet] = IndexedSeq.empty
    }

    @scala.annotation.tailrec
    private def processPackets(
      curArrival: Long,
      timeTP:     Long,
      st:         State
    ): State =
      st.buffer.headOption match {
        case Some(packet)
            if packet.duration <= timeTP & st.buffer.tail.nonEmpty =>
          processPackets(
            curArrival + packet.duration,
            timeTP - packet.duration,
            Worked(
              st.maxBufferSize,
              st.buffer.tail,
              st.processingLogs.:+(curArrival)
            )
          )
        case Some(packet)
            if packet.duration <= timeTP & st.buffer.tail.isEmpty =>
          Vacant(st.maxBufferSize, st.processingLogs.:+(curArrival))
        case Some(packet) if packet.duration > timeTP =>
          Worked(
            st.maxBufferSize,
            st.buffer
              .updated(0, packet.copy(duration = packet.duration - timeTP)),
            st.processingLogs
          )
        case None =>
          Vacant(st.maxBufferSize, st.processingLogs)
      }
    def process(state: State, next: Packet): State = state match {
      case w @ Worked(_, buffer, _) =>
        val current = buffer.head
        val res = processPackets(
          next.arrival,
          next.arrival - current.arrival,
          w
        )
        res.add(next, res.processingLogs)
      case v: Vacant =>
        processPackets(
          next.arrival,
          next.arrival,
          Worked(v.maxBufferSize, IndexedSeq(next), v.processingLogs)
        )

    }
  }

  import State._
  def process(
    bufferSize:          Int,
    packetsCount:        Int,
    arrivalWithDuration: IndexedSeq[(Long, Long)]
  ): State =
    if (arrivalWithDuration.nonEmpty)
      (0 until packetsCount).foldLeft(
        Vacant(bufferSize, IndexedSeq.empty): State
      ) {
        case (state, i) =>
          val (arrival, duration) = arrivalWithDuration(i)
          State.process(state, Packet(arrival, duration))
      } else {
      Vacant(bufferSize, IndexedSeq.empty)
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

}

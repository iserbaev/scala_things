package tasks

/**
  * По данным n процессорам и m задач определите, для каждой из задач, каким процессором она будет обработана.
  *
  * Вход. Число процессоров n и последовательность чисел t0, . . . , tm−1, где ti — время, необходимое на обработку i-й задачи.
  *
  * Выход. Для каждой задачи определите, какой процессор и в какое время начнёт её обрабатывать,
  * предполагая, что каждая задача поступает на обработку первому освободившемуся процессору.
  */
object Statement_6 {
  import scala.collection.mutable

  case class Processor(processorIndex: Int, startTime: Long, completeTime: Long)
  object Processor {
    val minDurationOrdering: Ordering[Processor] = new Ordering[Processor] {
      override def compare(self: Processor, that: Processor): Int =
        self.completeTime.compareTo(that.completeTime) match {
          case 0 =>
            self.processorIndex.compare(that.processorIndex) match {
              case 0 => 0
              case c => -c
            }
          case c => -c
        }
    }
  }
  case class Pool(size: Int) {
    private val builder =
      mutable.PriorityQueue.newBuilder[Processor](Processor.minDurationOrdering)
    (0 until size).map(i => builder += Processor(i, 0, 0))

    private val processors = builder.result()

    def addTask(duration: Int): Processor =
      if (duration > 0) {
        val completed = processors.dequeue()
        val task = Processor(
          processorIndex = completed.processorIndex,
          startTime      = completed.completeTime,
          completeTime   = completed.completeTime + duration
        )
        processors.enqueue(task)
        task
      } else {
        val available = processors.head
        val task = Processor(
          processorIndex = available.processorIndex,
          startTime      = available.completeTime,
          completeTime   = available.completeTime + duration
        )
        task
      }

    def getAll: Seq[Processor] =
      processors.dequeueAll
  }

}

object Main {
  import Statement_6._
  def main(args: Array[String]): Unit = {
    val (n, _) = {
      val ar = scala.io.StdIn.readLine().split(" ").map(_.toInt)
      ar.head -> ar.last
    }
    val tasks = scala.io.StdIn.readLine().split(" ").map(_.toInt)

    val pool = Pool(n)

    val tasksWithStartTime = tasks.map(pool.addTask)

    tasksWithStartTime.foreach(
      t => println(s"${t.processorIndex} ${t.startTime}")
    )
  }
}

object TestApp6 extends App {
  import Statement_6.Pool
  def test(n: Int, tasks: Array[Int], expected: Array[(Int, Int)]) = {
    val pool               = Pool(n)
    val tasksWithStartTime = tasks.map(pool.addTask)
    val result             = tasksWithStartTime.map(t => t.processorIndex -> t.startTime)

    expected.zipWithIndex.foreach {
      case ((procIndex, startTime), index) =>
        val (resProcIndex, resStartTime) = result(index)
        assert(
          resProcIndex == procIndex,
          s"$resProcIndex != $procIndex. index=$index, expectedProcIndex = $procIndex"
        )
        assert(
          resStartTime == startTime,
          s"$resStartTime != $startTime. index=$index, expectedStartTime = $startTime"
        )
    }
    println("success")
  }

  test(
    2,
    Array(1, 2, 3, 4, 5),
    Array(
      (0, 0),
      (1, 0),
      (0, 1),
      (1, 2),
      (0, 4)
    )
  )

  test(
    2,
    Array(0, 0, 1, 0, 0, 0, 2, 1, 2, 3, 0, 0, 0, 2, 1),
    Array(
      0 -> 0,
      0 -> 0,
      0 -> 0,
      1 -> 0,
      1 -> 0,
      1 -> 0,
      1 -> 0,
      0 -> 1,
      0 -> 2,
      1 -> 2,
      0 -> 4,
      0 -> 4,
      0 -> 4,
      0 -> 4,
      1 -> 5
    )
  )
}

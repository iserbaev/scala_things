package tasks

import scala.collection.mutable.ArrayBuffer

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

  case class Processor(processorIndex: Int, startTime: Int, completeTime: Int)
  object Processor {
    val minDurationOrdering: Ordering[Processor] =
      Ordering.by[Processor, Int](_.completeTime)(Ordering.Int.reverse)
  }
  case class Pool(size: Int) {
    private var availableProcessorIndex = 0
    private val tasksTimes =
      mutable.PriorityQueue.empty[Processor](Processor.minDurationOrdering)

    private def isAvailable: Boolean = availableProcessorIndex < size

    def addTask(duration: Int): Processor =
      if (isAvailable) {
        val task = Processor(
          processorIndex = availableProcessorIndex,
          startTime      = 0,
          completeTime   = duration
        )
        tasksTimes.enqueue(task)
        if (duration > 0) {
          availableProcessorIndex += 1
        }
        task
      } else {
        val completed = tasksTimes.dequeue()
        val task = Processor(
          processorIndex = completed.processorIndex,
          startTime      = completed.completeTime,
          completeTime   = completed.completeTime + duration
        )
        tasksTimes.enqueue(task)
        task
      }

    def getAll: Seq[Processor] =
      tasksTimes.dequeueAll
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

import java.util.concurrent.RecursiveTask
import scala.util.DynamicVariable

object common {
  val forkJoinPool = new java.util.concurrent.ForkJoinPool

  abstract class TaskScheduler {
    def schedule[T](body: => T): java.util.concurrent.ForkJoinTask[T]
    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): java.util.concurrent.ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      forkJoinPool.execute(t)
      t
    }
  }

  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  def task[T](body: => T): java.util.concurrent.ForkJoinTask[T] =
    scheduler.value.schedule(body)

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
    scheduler.value.parallel(taskA, taskB)

  def parallel[A, B, C, D](
      taskA: => A,
      taskB: => B,
      taskC: => C,
      taskD: => D
  ): (A, B, C, D) = {
    val ta = task(taskA)
    val tb = task(taskB)
    val tc = task(taskC)
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }

}

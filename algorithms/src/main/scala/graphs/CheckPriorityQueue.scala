package graphs

object CheckPriorityQueue extends App {
  val distances      = Array(0, 12, 11, 10)
  val customOrdering = Ordering.by[Int, Int](i => distances(i)).reverse
  val priorityQueue  = scala.collection.mutable.PriorityQueue.empty[Int](customOrdering) // ASC heap
  priorityQueue.addAll(Seq(0, 1, 2, 3))

  while (priorityQueue.nonEmpty){
    val u = priorityQueue.dequeue()
    println(u)

    distances.update(2,1)
    if (priorityQueue.nonEmpty) {
      val v = priorityQueue.dequeue()
      priorityQueue.enqueue(v)
    }
  }

}

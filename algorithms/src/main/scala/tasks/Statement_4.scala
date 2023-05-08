package tasks

object Statement_4 {
  class StackMax {
    private var underlying = List.empty[(Int, Int)]
    private var max_v      = Int.MinValue

    def push(v: Int): Unit = {
      max_v = max_v.max(v)
      underlying = (v, max_v) :: underlying
    }

    def pop(): Unit = {
      underlying = underlying.tail
      max_v = underlying.head._2
    }

    def max(): Unit =
      println(max_v)
  }
}

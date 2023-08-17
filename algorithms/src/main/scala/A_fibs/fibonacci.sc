import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable.ListBuffer

def fibFolding(n: Long): ListBuffer[Long] =
  if (n <= 1) ListBuffer(0)
  else {
    val lb                         = ListBuffer(0L, 1L)
    val range: immutable.Seq[Long] = 2L to n
    range.foldLeft((0L, 1L)) { case ((prev, curr), _) =>
      val next = prev + curr
      lb.append(next)
      (curr, next)
    }
    lb
  }

def periodPisano(m: Long): ListBuffer[Long] = {
  @tailrec def recur(periodBuffer: ListBuffer[Long], a: Long, b: Long, pa: Long, pb: Long): ListBuffer[Long] =
    (pa, pb) match {
      case (0, 1) => periodBuffer
      case _ =>
        recur(periodBuffer += pb, b, a + b, pb, (a + b) % m)
    }

  if (m == 1) ListBuffer(0) else recur(ListBuffer(0, 1), 1, 1, 1, 1).init
}

5 % 5

periodPisano(1)
periodPisano(2)
periodPisano(3)
periodPisano(4)
periodPisano(5)
periodPisano(6)
periodPisano(16)
periodPisano(67)
fibFolding(67)
fibFolding(20)

Map(1 -> 1, 2 -> 3, 3 -> 8, 4 -> 6, 5 -> 20, 6 -> 24, 16 -> 24, 67 -> 136).foreach { case (k, v) =>
  assert(periodPisano(k.toLong).size == v, s"p($k) != $v, but ${periodPisano(k.toLong)}")
}

import java.util.concurrent.{ForkJoinPool, ForkJoinTask, ForkJoinWorkerThread, RecursiveTask}

import scala.util.DynamicVariable

val forkJoinPool = new ForkJoinPool
abstract class TaskScheduler {
  def schedule[T](body: => T): ForkJoinTask[T]
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())
  }
}
class DefaultTaskScheduler extends TaskScheduler {
  def schedule[T](body: => T): ForkJoinTask[T] = {
    val t = new RecursiveTask[T] {
      def compute = body
    }
    Thread.currentThread match {
      case wt: ForkJoinWorkerThread => t.fork()
      case _ => forkJoinPool.execute(t)
    }
    t
  }
}
val scheduler = new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)
def task[T](body: => T): ForkJoinTask[T] = {
  scheduler.value.schedule(body)
}
def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
  scheduler.value.parallel(taskA, taskB)
}
def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
  val ta=task{taskA}
  val tb=task{taskB}
  val tc=task{taskC}
  val td = taskD
  (ta.join(), tb.join(), tc.join(), td)
}

def scanLeft[A](inp: Array[A],
                a0: A, f: (A,A) => A,
                out: Array[A]): Unit = {
  out(0) = a0
  var a = a0
  var i = 0
  while (i < inp.length) {
    a = f(a,inp(i))
    i = i + 1
    out(i) = a
  }
}

def reduceSeg1[A](inp: Array[A], left: Int, right: Int,
                  a0: A, f: (A,A) => A): A = {
  inp(0)
}
def mapSeg[A,B](inp: Array[A], left: Int, right: Int,
                fi : (Int,A) => B,
                out: Array[B]): Unit = {}

List(1,3,8,11,16).scanLeft(100)(_ + _)

val inp = Array(1,3,8,11,16)
val out = Array[Int]()

scanLeft[Int](inp, 100, (a: Int, b: Int) => a + b, out)

def scanLeft2[A](inp: Array[A], a0: A, f: (A,A) => A, out: Array[A]) = {
  val fi = { (i:Int, v:A) => reduceSeg1(inp, 0, i, a0, f) }
  mapSeg(inp, 0, inp.length, fi, out)
  val last = inp.length - 1
  out(last + 1) = f(out(last), inp(last))
}

//Trees storing our input collection only have values in leaves:
sealed abstract class Tree[A]
case class Leaf[A](a: A) extends Tree[A]
case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]
//Trees storing intermediate values also have (res) values in nodes:
sealed abstract class TreeRes[A] { val res: A }
case class LeafRes[A](override val res: A) extends TreeRes[A]
case class NodeRes[A](l: TreeRes[A],
                      override val res: A,
                      r: TreeRes[A]) extends TreeRes[A]

def reduceRes[A](t: Tree[A], f: (A,A) => A): TreeRes[A] = {
  t match {
    case Leaf(a) => LeafRes(a)
    case Node(l , r) => {
      val (tL, tR) = (reduceRes(l, f), reduceRes(r, f) )
      NodeRes(tL, f(tL.res, tR.res), tR)
    }
  }
}

def upsweep[A](t: Tree[A], f: (A,A) => A): TreeRes[A] = t match {
  case Leaf(v) => LeafRes(v)
  case Node(l, r) => {
    val (tL, tR) = parallel(upsweep(l, f), upsweep(r, f))
    NodeRes(tL, f(tL.res, tR.res), tR)
  }
}

// ’a0’ is reduce of all elements left of the tree ’t’
def downsweep[A](t: TreeRes[A], a0: A, f : (A,A) => A): Tree[A] = t match {
  case LeafRes(a) => Leaf(f(a0, a))
  case NodeRes(l, _, r) => {
    val (tL, tR) = parallel(downsweep[A](l, a0, f),
      downsweep[A](r, f(a0, l.res), f))
    Node(tL, tR)
  }
}

def scanLeft[A](t: Tree[A], a0: A, f: (A,A) => A): Tree[A] = {
  val tRes = upsweep(t, f)
  val scan1 = downsweep(tRes, a0, f)
  prepend(a0, scan1)
}

def prepend[A](x: A, t: Tree[A]): Tree[A] = t match {
  case Leaf(v) => Node(Leaf(x), Leaf(v))
  case Node(l, r) => Node(prepend(x, l), r)
}
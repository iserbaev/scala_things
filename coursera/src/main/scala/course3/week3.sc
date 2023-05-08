import java.util.concurrent.ConcurrentSkipListSet

//import scala.collection.parallel.Task
import scala.collection.{GenSet, mutable}

def initializeArray(xs: Array[Int])(v: Int): Unit = {
  for (i <- (0 until xs.length).par) {
    xs(i) = v + i
  }
}


val xs = new Array[Int](1000001)
(1 to 1000000).foreach(i => xs(i) = i)
var start = System.currentTimeMillis()
initializeArray(xs)(25)
var end = System.currentTimeMillis()
var dur = end - start


xs

/**
  * Mandelbrot set
 */

/**
private def computePixel(xc: Double, yc: Double, maxIterations: Int): Int = {
  var i = 0
  var x, y = 0.0
  while (x * x + y * y < 4 && i < maxIterations) {
    val xt = x * x - y * y + xc
    val yt = 2 * x * y + yc
    x = xt; y = yt
    i += 1
  }
  color(i)
}

def parRender(): Unit = {
  for (idx <- (0 until image.length).par) {
    val (xc, yc) = coordinatesFor(idx)
    image(idx) = computePixel(xc, yc, maxIterations)
  }
}
*/

/**
  * 2
  */

def sum1 (xs: Array[Int]): Int = {
  xs.par.foldLeft(0)(_ + _)  // NO PARALLEL!
}

def sum2 (xs: Array[Int]): Int = {
  xs.par.fold(0)(_ + _)  // IT IS THE PARALLEL!
}

start = System.currentTimeMillis()
sum1(xs)
end = System.currentTimeMillis()
dur = end - start

start = System.currentTimeMillis()
sum2(xs)
end = System.currentTimeMillis()
dur = end - start

/**
  * 3
  */

def max(xs: Array[Int]): Int = {
  xs.par.fold(Int.MinValue)(math.max)
}

max(xs)

val ar = Array("paper", "rock", "paper", "scissors")
def play(a: String, b: String): String = List(a, b).sorted match {
  case List("paper", "scissors") => "scissors"
  case List("paper", "rock")     => "paper"
  case List("rock", "scissors")  => "rock"
  case List(a, b) if a == b      => a
  case List("", b)               => b
}

ar.par.fold("")(play) // answer = scissors
ar.fold("")(play) // answer = paper

// The play operator is commutative, but not associative.

/**
  *
  * f(a, f(b, c)) == f(f(a, b), c)
  * f(z, a) == f(a, z) == a
  * We say that the neutral element z and the binary operator f must form a monoid.
  */

/**
  * Array('E', 'P', 'F', 'L').par
  * .fold(0)((count, c) => if (isVowel(c)) count + 1 else count)
  *
  * DON'T COMPILE because 0 is Int and 'E' is Char
  *
  * The fold operation can only produce values of the same type as the collection that
  * it is called on.
  */

def isVowel(c: Char): Boolean =
  List('a', 'e', 'i', 'o', 'u') contains Character.toLowerCase(c)

/**
  * Using aggregate can skip this limitation
  */
Array('E', 'P', 'F', 'L').par.aggregate(0)(
  (count, c) => if (isVowel(c)) count + 1 else count, _+_
)

/** 4
  */

// wrong version!
def intersection1(a: GenSet[Int], b: GenSet[Int]): mutable.Set[Int] = {
  val result = mutable.Set[Int]() // is not thread safe!
  for (x <- a) if (b contains x) result += x
  result
}
intersection1((0 until 1000).toSet, (0 until 1000 by 4).toSet)
intersection1((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet) // non determenistic results


def intersection2(a: GenSet[Int], b: GenSet[Int]) = {
  val result = new ConcurrentSkipListSet[Int]()
  for (x <- a) if (b contains x) result.add(x)
  result
}
intersection2((0 until 1000).toSet, (0 until 1000 by 4).toSet)
intersection2((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)

//avoiding side-effect
def intersection(a: GenSet[Int], b: GenSet[Int]): GenSet[Int] = {
  if (a.size < b.size) a.filter(b(_)) else b.filter(a(_))
}
intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet)
intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)

//Rule: Never modify a parallel collection on which a data-parallel operation is in progress.
val graph = mutable.Map[Int, Int]() ++= (0 until 100000).map(i => (i, i + 1))
graph(graph.size - 1) = 0
for ((k, v) <- graph.par) graph(k) = graph(v)
val violation = graph.find({ case (i, v) => v != (i + 2) % graph.size })
println(s"violation: $violation")

/**
  * Never write to a collection that is concurrently traversed.
  * Never read from a collection that is concurrently modified.
  */


/**TrieMap is an exception to these rules.
  * The snapshot method can be used to efficiently grab the current state:
  */
val graph2 = collection.concurrent.TrieMap[Int, Int]() ++= (0 until 100000).map(i => (i, i + 1))
graph2(graph.size - 1) = 0
val previous = graph2.snapshot()
for ((k, v) <- graph2.par) graph(k) = previous(v)
val violation2 = graph2.find({ case (i, v) => v != (i + 2) % graph.size })
println(s"violation: $violation2")

/** 6
  * iterators
  * splitters
  * builders
  * combiners
  */

trait Iterator[T] {
  def hasNext: Boolean
  def next(): T
  def foldLeft[S](z: S)(f: (S, T) => S): S = {
    var result = z
    while (hasNext) {
      f(result, next())
    }
    result
  }
}

import TaskBuilder._

trait Splitter[T] extends Iterator[T]{
  def split: Seq[Splitter[T]]
  def remaining: Int
  def threshold: Int
  def fold(z: T)(f: (T, T) => T): T = {
    if (remaining < threshold) foldLeft(z)(f)
    else {
      val children = split.map(x => task(fold(z)(f)))
      children.map(_.join()).foldLeft(z)(f)
    }
  }
}

trait Traversable[T] {
  def foreach(f: T => Unit): Unit
  def newBuilder: mutable.Builder[T, Traversable[T]]
  def filter(p: T => Boolean): Traversable[T] = {
    val b = newBuilder
    foreach ( el => if (p(el)) b += el )
    b.result()
  }
}
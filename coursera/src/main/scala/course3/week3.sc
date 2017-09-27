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
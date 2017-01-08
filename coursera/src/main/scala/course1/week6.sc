/**
  * Usual operations for Seq - as map, filter, fold, head, tail, take , take,
  * takeWhile
  * ALSO works for String and Array (and also all methods for Iterable trait!)
  */

val xs = Array(1,2,3,44)
val ys = xs map (x => x * 2)

val s = "Hello World"
s filter (c => c.isUpper)
s.head
s.tail
s takeWhile(c => c.isUpper)
s.indexWhere(c => c.isLower)
s.padTo(16,"a")
s.reverse
s.sorted

/**
  *
  */

val r: Range = 1 until 5
val t: Range = 1 to 5

1 to 10 by 3
6 to 1 by -2

/**
  * also next operations
  */
xs exists(_ == 2)
xs forall(_ == 2)
val zs = xs zip ys
zs.unzip
xs flatMap(x => Array(x,x+x, x*x))
xs.sum
xs.product

xs.max
xs.min

s.max

val pairs = ((1 to 11) zip s).toList
pairs.unzip

s flatMap(c => List('.', c))

/**
  * list all combination of x and y
  * where x is drawn from 1 .. M and y is drawn from 1..N
  */
def combinations(M: Int, N:Int) :IndexedSeq[(Int, Int)] = {
  (1 to M) flatMap( x => (1 to N) map(y => (x,y)))
}

val res = combinations(10, 5)
res.length

/**
  * compute scalar product of two vectors
  */
def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map(xy => xy._1*xy._2).sum

scalarProduct(Vector(1,2,3), Vector(4,5,6))

/**
  * alternative way with pattern matching pattern
  */
def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map { case (x,y) => x*y }.sum

scalarProduct2(Vector(1,2,3), Vector(4,5,6))

def isPrime(n: Int): Boolean =
//  !(2 until n).exists(x => (n % x) == 0)
  (2 until n).forall(x => n%x != 0)

isPrime(5)
isPrime(4)
isPrime(2017)

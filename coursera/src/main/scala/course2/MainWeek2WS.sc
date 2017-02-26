import course2.MainWeek2.Pouring

def isPrime(n: Int): Boolean =
  (2 until n).forall(x => n%x != 0)


((1000 to 10000) filter isPrime)(1)

val xs = Stream.cons(1, Stream.cons(2, Stream.empty))
val ys = Stream(1,2,3)

((1000 to 10000).toStream filter isPrime)(1)

def from(n: Int): Stream[Int] = n #:: from(n+1)
val naturals = from(0)
val m4s = naturals map (_ * 4)
(m4s take 100).toList

def sieve(s: Stream[Int]): Stream[Int] =
s.head #:: sieve(s.tail filter (_ % s.head != 0))

val primes = sieve(from(2))

(primes take 10).toList

def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double): Double = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

sqrtStream(4).take(10).toList

def isGoodEnough(guess: Double, x: Double) =
  math.abs((guess*guess - x) / x) < 0.0001

sqrtStream(4).filter(isGoodEnough(_, 4)).take(10).toList


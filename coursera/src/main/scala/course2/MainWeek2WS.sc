def isPrime(n: Int): Boolean =
  (2 until n).forall(x => n%x != 0)


((1000 to 10000) filter isPrime)(1)

val xs = Stream.cons(1, Stream.cons(2, Stream.empty))
val ys = Stream(1,2,3)

((1000 to 10000).toStream filter isPrime)(1)
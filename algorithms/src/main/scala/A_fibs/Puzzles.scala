package A_fibs

import java.io.{ BufferedReader, InputStreamReader }

object MaxPairwise {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )
    br.readLine().toInt
    br.readLine().split(" ").map(_.toLong).toList match {
      case List()     => println(0)
      case List(a)    => println(a)
      case List(a, b) => println(a * b)
      case list =>
        val sorted = list.sorted(Ordering.Long.reverse)
        println(sorted.head * sorted.tail.head)
    }

  }
}

object FibonacciMemoized {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )
    val n = br.readLine().toInt

    val res = fibonacciMemoized(n)
    println(res)
  }

  //Fn = Fn-1 + Fn-2
  def fibonacciMemoized(n: Int): Int = {
    val acc = Array.fill(n + 2)(0)
    acc.update(1, 1)

    @scala.annotation.tailrec
    def recur(prev: Int, current: Int, ac: Array[Int]): Int =
      if (current >= n) {
        ac(n)
      } else {
        recur(current, current + 1, ac.updated(current + 1, ac(current) + ac(prev)))
      }

    recur(0, 1, acc)
  }
}

object FibonacciRecursive {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )
    val n = br.readLine().toInt

    val res = fibonacciRecursive(n)
    println(res)
  }

  //Fn = Fn-1 + Fn-2
  def fibonacciRecursive(n: Int): Int = {
    @scala.annotation.tailrec
    def recur(prev: Int, current: Int, idx: Int): Int =
      if (idx > n) {
        prev
      } else if (idx == n) {
        current
      } else {
        recur(current, current + prev, idx + 1)
      }

    recur(0, 1, 1)
  }
}

object FibonacciLastDigit {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )
    val n = br.readLine().toInt

    val res = fibonacciRecursiveModulo(n)
    println(res)
  }

  //Fn = Fn-1 + Fn-2
  def fibonacciRecursiveModulo(n: Int): Int = {
    @scala.annotation.tailrec
    def recur(prev: Int, current: Int, idx: Int): Int =
      if (idx > n) {
        prev % 10
      } else if (idx == n) {
        current % 10
      } else {
        recur(current, current + prev % 10, idx + 1)
      }

    recur(0, 1, 1)
  }
}

object FibonacciModulo {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )
    val ar     = br.readLine().split(" ").map(_.toLong)
    val n      = ar.head
    val modulo = ar.last

    val res = fibonacciRecursiveModulo(n, modulo)
    println(res)
  }

  def pisano(modulo: Long): Long = {
    var prev    = 0L
    var current = 1L
    var result  = 0L

    (0L to modulo * modulo).find { idx =>
      val temp = prev
      prev = current
      current = (temp + current) % modulo
      if (prev == 0L && current == 1L) result = idx + 1
      prev == 0L && current == 1L
    }

    result
  }

  //Fn = Fn-1 + Fn-2
  def fibonacciRecursiveModulo(num: Long, mod: Long): Long = {
    val pisanoPeriod = pisano(mod)

    val n = num % pisanoPeriod

    var prev = 0L
    var curr = 1L

    if (n == 0L) 0L
    else if (n == 1L) 1L
    else {
      var i = 0L
      while (i < n - 1) {
        var temp = 0L
        temp = curr
        curr = (prev + curr) % mod
        prev = temp

        i += 1
      }
      curr % mod
    }
  }
}

object FibonacciLastDigitSum {
  def main(args: Array[String]): Unit = {
    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    val n = br.readLine().toLong

    println(sumLastDigit(n))
  }

  private def sumLastDigit(n: Long): Long =
    if (n <= 1) n else (fibonacciLastDigit((n + 2) % 60) + 9) % 10

  def fibonacciLastDigit(n: Long) = {
    var current = 0L
    var next    = 1L

    (1L to n).foreach { _ =>
      val temp = next
      next = (current + next) % 10
      current = temp
    }
    current
  }
}

object FibonacciLastDigitPartialSum {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    val ar = br.readLine().split(" ").map(_.toLong)
    val m  = ar.head
    val n  = ar.last
    if (m + n < 2) println(m + n)
    else {
      val partialSum = sumLastDigit(n) - sumLastDigit(math.max(0, m - 1))

      println((if (partialSum > 0) partialSum else (10 + partialSum)) % 10)
    }
  }

  private def sumLastDigit(n: Long): Long =
    if (n <= 1) n else (fibonacciLastDigit((n + 2) % 60) + 9) % 10

  private def fibonacciLastDigit(n: Long): Long = {
    var current = 0L
    var next    = 1L

    (1L to n).foreach { _ =>
      val temp = next
      next = (current + next) % 10
      current = temp
    }
    current
  }
}

object FibonacciLastDigitSumOfSquares {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    val n = br.readLine().toLong

    println(sumLastDigitOfSquares(n))
  }

  private def sumLastDigitOfSquares(n: Long): Long =
    if (n <= 1) n else fibonacciSumSquares(n % 60) % 10

  // FSQn = Fn * Fn+1
  def fibonacciSumSquares(n: Long) = {
    var current = 0L
    var next    = 1L

    (1L to n).foreach { _ =>
      val temp = next
      next = (current + next) % 10
      current = temp
    }
    (current * next) % 10
  }
}

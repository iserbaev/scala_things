package tasks

//5*5 = 5^2 = 25 ==> SquareRoot(25) = 5
//SquareRoot(42)  = 6.4807......
object Solution extends App {
  // Approximate the square root of a number.
  val result = SquareRoot.calculate(42, 0.001)

  println("Square root is : " + result)

}

object SquareRoot {

  // number - number for which the square root should be found
  // eps - precision of the answer i.e = .001
  // SquarRoot(25) = 5
  // SquarRoot(49) = 7

  // SquareRoot(42) = (0, 42] => 6.4807

  // (0, 42]: (0+42)/2 = 21 => 21*21 > 42
  //  (0, 21] :
  //   (0, 10.5]
  //    (5.25, 10.5)

  // SquareRoot(35) = (0, 35]:  x in (0, 35]

  def calculate(number: Double, eps: Double): Double = {
    @scala.annotation.tailrec
    def recur(start: Double, end: Double, counter: Int = 0): Double = {
      println(s"start $start, end: $end, counter: $counter")
      val halfSum = (start + end) / 2
      val mult    = halfSum * halfSum

      val isValid = math.abs(number - mult) <= eps

      if (isValid) mult
      else if (mult > number) recur(start, halfSum, counter + 1)
      else recur(halfSum, end, counter + 1)
    }

    recur(0, number)
  }
}

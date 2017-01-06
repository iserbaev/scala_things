/**
  * Created by ilnur on 06.01.17.
  */
object wk5_3 extends App{
  def scaleList(xs: List[Double], factor: Double): List[Double] = xs match {
    case Nil => xs
    case y :: ys => y * factor :: scaleList(ys, factor)
  }
  def scaleList2(xs: List[Double], factor: Double): List[Double] = {
    xs map(x => x * factor)
  }

  def squareList(xs: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case y :: ys => y * y :: squareList(ys)
    }

  def squareList2(xs: List[Int]): List[Int] =
    xs map(x => x * x)

  val nums = List(2,-4,5,7,1)

  nums filter(x => x > 0)
  nums filterNot(x => x > 0)
  nums partition(x => x > 0)

  nums takeWhile(x => x > 0)
  nums dropWhile(x => x > 0)
  nums span(x => x > 0)


  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span(y => y == x)
      first :: pack(rest)
  }
  val data = List("a", "a", "a", "b", "c", "c", "a")
  print(pack(data))

  def encode[T](xs: List[T]): List[(T, Int)] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span(y => y == x)
      (x, first.length) :: encode(rest)
  }
  def encode2[T](xs: List[T]): List[(T, Int)] = pack(xs) map (ys => (ys.head, ys.length))
  print(encode(data))
}

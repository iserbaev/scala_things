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
  println(pack(data))

  def encode[T](xs: List[T]): List[(T, Int)] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span(y => y == x)
      (x, first.length) :: encode(rest)
  }
  def encode2[T](xs: List[T]): List[(T, Int)] = pack(xs) map (ys => (ys.head, ys.length))
  println(encode(data))

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case y :: ys => y + sum(ys)
  }
  def product(xs: List[Int]): Int = xs match {
    case Nil => 1
    case y :: ys => y * product(ys)
  }

//  def reduceLeft(op: (T, T) => T): T = this match {
//    case Nil => throw new Error("Nil reduceLeft")
//    case x :: xs => (xs foldLeft x)(op)
//  }
//
//  def foldLeft[U](z: U)(op: (U, T) => U): U = this match {
//    case Nil => z
//    case x :: xs => (xs foldLeft op(z,x))(op)
//  }
//  def reduceRight(op: (T, T) => T): T = this match {
//    case Nil => throw new Error("Nil reduceRight")
//    case x :: Nil => x
//    case x :: xs => op(x, xs.reduceRight(op))
//  }
//  def foldRight[U](z: U)(op: (U, T) => U): U = this match {
//    case Nil => z
//    case x :: xs => op(x, (xs foldRight(z))(op))
//  }

  def sum2(xs: List[Int]): Int = (0 :: xs) reduceLeft(_ + _)
  def product2(xs: List[Int]): Int = (1 :: xs) reduceLeft(_ * _)

  def sum3(xs: List[Int]): Int = (xs foldLeft 0)(_ + _)
  def product3(xs: List[Int]): Int = (xs foldLeft 1)( _ * _)

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())((x, y) => y ++ List(f(x)))

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((x,y) => if (x == Nil) y else y+1)

  println(nums)
  println(lengthFun(data))
  println(mapFun[Int, Int](nums, x => x*2))
}

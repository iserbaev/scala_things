package tasks_hostmann.ch14

/**
  * Created by ilnur on 07.12.16.
  */
abstract class Item
case class Article(description: String, price:    Double) extends Item
case class Bundle(description:  String, discount: Double, items: Item*)
    extends Item
case class Product(description: String, price: Double) extends Item
case class Multiple(factor:     Int, items:    Item) extends Item

object MainCH14 extends App {

  /**
    * ch14_tsk2
    */
  def swapWithCase[T](x: T, y: T): (T, T) =
    x match {
      case _ => y -> x
    }
  println(swapWithCase("a", "b"))
  println(swapWithCase(4, 5))

  /**
    * ch14_tsk3
    */
  def swapArr[T](arr: Array[T]): Array[T] =
    arr match {
      case _ if arr.length >= 2 =>
        ; val x = arr; val y = arr(0); x.update(0, arr(1)); x.update(1, y); x
      case _ => arr
    }
  println(swapArr(Array(1, 2, 3)).toBuffer.toString())
  println(swapArr(Array('d', 'f', 'g')).toBuffer.toString())
  val x = List(3, 4)
  5 :: List(5)

  /**
    *ch14_tsk4
    */
  def price(it: Item): Double = it match {
    case Article(_, p)             => p
    case Bundle(_, disc, its @ _*) => its.map(price).sum - disc
    case Product(_, p)             => p
    case Multiple(factor, item)    => factor * price(item)
  }
}

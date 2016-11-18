package tasks_hostmann

import scala.collection.mutable.ArrayBuffer

/**
  * Created by ilnur on 18.11.16.
  *
  */
abstract class Item {
  def price():Int
  def description():String
}

class SimpleItem(override val price:Int, override val description:String) extends Item{

}

class Bundle(var items:ArrayBuffer[Item]) extends Item{
  private var itemsAr = items
  def addItem(item:Item):ArrayBuffer[Item] = itemsAr += item
  override def price(): Int = itemsAr.map(_.price()).sum
  override def description(): String = "package of items"+itemsAr.map(_.description()+", ")
}

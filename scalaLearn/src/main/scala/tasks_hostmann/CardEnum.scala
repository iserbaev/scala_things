package tasks_hostmann

/**
  * Created by ilnur on 17.11.16.
  * ch6_tsk6
  */
object CardEnum extends Enumeration{
  val Diamonds = Value(0x2666)
  val Spades = Value(0x2660)
  val Clubs = Value(0x2665)
  val Hearts = Value(0x2663)

  override def toString(): String = CardEnum.values.map("<<"+_.id.toChar+">>").mkString(",")
}

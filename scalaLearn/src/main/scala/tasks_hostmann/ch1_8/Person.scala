package tasks_hostmann.ch1_8

import scala.collection.mutable.ArrayBuffer

/**
  * Created by ilnur on 17.11.16.
  * ch5_tsk7
  * ch8_tsk8
  */
class Person(val fullName: String) extends Serializable {
//  val firstName=fullName.split(" ")(0)
//  val lastName=fullName.split(" ")(1)
  var friends = new ArrayBuffer[Person]()
  override def toString: String = getClass.getName + "[name=" + fullName + "]"
}

class SecretAgent(codeName: String) extends Person(codeName) {
  override val fullName = "secret"
  override def toString: String = "secret"
}

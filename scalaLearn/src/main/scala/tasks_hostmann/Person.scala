package tasks_hostmann

/**
  * Created by ilnur on 17.11.16.
  * ch5_tsk7
  */
class Person(val fullName:String) {
  val firstName=fullName.split(" ")(0)
  val lastName=fullName.split(" ")(1)
  println(firstName+"\n"+lastName+"\n")
}

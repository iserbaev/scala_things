package tasks_hostmann.scala-impatient-code.code.ch14.sec14

case class Euro(value: Double) extends Amount
  // This class doesn't compile since Amount is sealed in another file

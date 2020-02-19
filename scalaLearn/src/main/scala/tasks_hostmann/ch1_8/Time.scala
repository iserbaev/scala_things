package tasks_hostmann.ch1_8

/**
  * Created by ilnur on 17.11.16.
  * ch5_tsk3
  * ch5_tsk4
  */
class Time(private var hours: Int, private var minutes: Int) {
  private val innerField = (hours % 24) * 60 + minutes % 60
  def before(thisT: Time, otherT: Time): Boolean =
    (thisT.hours * 60 + thisT.minutes) < (otherT.hours * 60 + otherT.minutes)
  hours   = innerField / 60
  minutes = innerField % 60
  def hour = hours
  override def toString: String =
    "hours:" + this.hours + ", minutes: " + this.minutes
}

object Time {
  def apply(hours: Int, minutes: Int) = new Time(hours, minutes)
}

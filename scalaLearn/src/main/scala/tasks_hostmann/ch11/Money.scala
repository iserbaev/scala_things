package tasks_hostmann.ch11

/**
  * Created by ilnur on 30.11.16.
  * ch11_tsk4
  */
class Money(val dollar: Int, val cents: Int) {
  def +(other: Money): Money =
    Money(
      this.dollar + other.dollar + (this.cents + other.cents) / 100,
      (this.cents + other.cents) % 100
    )
  def -(other: Money): Money =
    Money(
      math.abs(this.dollar - other.dollar),
      math.abs(this.cents - other.cents)
    )
  def ==(other: Money): Boolean =
    this.dollar == other.dollar && this.cents == other.cents
  def <(other: Money): Boolean =
    (this.cents + this.dollar * 100) < (other.cents + other.dollar * 100)

  def >(other: Money): Boolean =
    other < this
}

object Money {
  def apply(dollar: Int, cents: Int): Money = new Money(dollar, cents)
}

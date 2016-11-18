package tasks_hostmann

/**
  * Created by ilnur on 17.11.16.
  * ch5_tsk2
  * ch8_tsk1
  * ch8_tsk2
  */
class BankAccount(initialBalance: Double) {
  private var balance=initialBalance
  def deposit(amount:Double):Double = {balance += amount; balance}
  def withdraw(amount:Double):Double = {balance -= amount; balance}
}

class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance){
  override def deposit(amount: Double): Double = super.deposit(amount)-1
  override def withdraw(amount: Double): Double = super.withdraw(amount)-1
}

class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance){
  private var balance = initialBalance
  private var transactionCount=3
  def earnMonthlyInterest(interest:Double):Double = {
    balance += interest
    transactionCount=0
    balance
  }

  override def deposit(amount: Double): Double = {
    if (transactionCount<3) super.deposit(amount)
    else super.deposit(amount)-1
    }

  override def withdraw(amount: Double): Double = {
    if (transactionCount<3) super.withdraw(amount)
    else super.withdraw(amount)-1
  }
}

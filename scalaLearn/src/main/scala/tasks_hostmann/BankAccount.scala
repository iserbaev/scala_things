package tasks_hostmann

/**
  * Created by ilnur on 17.11.16.
  * ch5_tsk2
  */
class BankAccount {
  private var balance=0
  def deposit(amount:Int):Unit = balance += amount
  def withdraw(amount:Int):Unit = balance -= amount
  def current = balance
}

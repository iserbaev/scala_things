/**
  * 1. Stateful object
  * For a stateful object,
  * the result of a method call or field access may depend on
  * what operations were previously performed on the object.
  */

/**
  * substitution model
  */

def iterate(n: Int, f: Int => Int, x: Int): Int =
  if (n == 0) x else iterate(n-1, f, f(x))

def square(x: Int): Int = x * x

iterate(10, square, 3)

/**
  * Теорема Чёрча-Россера
  * Для любого выражения E и любых двух последовательностей редукции E -> E1 и E-> E2
  * существуют две последовательности редукции E1 -> E3 и E2-> E3,
  * приводящие к одному и тому же результату E3.
  */

/**
  * An object has a state if his behavior influenced by his history
  */

/**
  * Example of object with state
  */

class BancAccount{
  private var balance = 0
  def deposit(amount: Int): Unit = {
    if (amount > 0) balance = balance + amount
  }

  def withdraw(amount: Int): Int = {
    if (0 < amount && amount <= balance) {
      balance = balance - amount
      balance
    } else throw new Error("insufficient funds")
  }
}

val acct = new BancAccount

acct deposit 50
acct withdraw 20
acct withdraw 20
acct withdraw 15

/**
  * 2. Identity and change
  */

/**
  * referential transparency
  * operational equivalence - x and y are operational equivalent if no possible test can distinguish between them
  */
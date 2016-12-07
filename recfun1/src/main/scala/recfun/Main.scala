package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("())(".toList))
    val list = List(1,2)
    println(countChange(4,list))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c==0 || r==0 || c==r) 1
      else pascal(c-1,r-1)+pascal(c,r-1)
    }

  def getBalance(chars: List[Char], count:Int, tr:Boolean):Boolean = {
    var res = tr
    var countInternal = count
    val head = chars.head
    val tail = chars.tail
    if (head.equals('(') && countInternal==0) {
      res=false
      countInternal = 1
    }
    if (head.equals(')') && countInternal==1) {
      res = true
      countInternal=0
    }
    if (head.equals(')') && countInternal==0 && !res) {
      res = false
      countInternal=2
    }
    if (countInternal != 2 && (tail.contains('(') || tail.contains(')'))) getBalance(tail, countInternal, res)
    else res
  }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      if (chars.contains(')') && !chars.contains('(')) false
      else if (!chars.contains(')') && chars.contains('(')) false
      else getBalance(chars,0, tr = true)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money==0) 1
      else if (money<0 || coins.isEmpty) 0
      else countChange(money, coins.tail)+countChange(money-coins.head, coins)
    }
    def and(x:Boolean, y: => Boolean) = if (x) y else false
    def or(x:Boolean, y: => Boolean) = if (x) true else y
  }

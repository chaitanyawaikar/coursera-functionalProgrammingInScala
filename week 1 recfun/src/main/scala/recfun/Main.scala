package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (c==0 || c==r) 1 else pascal(c-1,r-1)+pascal(c,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = balance(chars, 0)
  
  def balance(chars: List[Char], parentheses: Int): Boolean =
    if (parentheses == -1) false
    else if (chars.isEmpty) parentheses == 0
    else balance(chars.tail, parentheses+getBalanceNumber(chars.head))
  
  def getBalanceNumber(head: Char): Int =
    if (head == '(') 1
    else if (head == ')') -1
    else 0

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money-coins.head, coins) + countChange(money, coins.tail)
}

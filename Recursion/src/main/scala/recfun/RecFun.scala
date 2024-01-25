package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    // Keep currOpen as the number of currently open braces (unmatched).
    @tailrec
    def balanceHelper(currOpen: Int, remChars: List[Char]): Boolean = {
      if (remChars.isEmpty) {
        // if we've reached the end its only valid if the number of currently opened
        // braces is 0.
        currOpen == 0
      } else if (remChars.head == '(') {
        // We got an open brace so increment the unmatched open count.
        balanceHelper(currOpen + 1, remChars.tail)
      } else if (remChars.head == ')') {
        // if we get a closing brace but no brace is currently open then it is not valid.
        if (currOpen <= 0) {
          false
        } else {
          // otherwise, we close the most recent open brace and continue.
          balanceHelper(currOpen - 1, remChars.tail)
        }
      } else {
        // otherwise the character is not a bracket.
        balanceHelper(currOpen, remChars.tail)
      }
    }

    balanceHelper(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    } else if (coins.isEmpty) {
      0
    } else if (coins.head > money) {
      countChange(money, coins.tail)
    } else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}

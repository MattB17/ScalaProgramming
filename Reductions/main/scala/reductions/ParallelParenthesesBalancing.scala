package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def balanceHelper(currOpen: Int, currIdx: Int): Boolean = {
      if (currIdx >= chars.length) {
        // We've reached the end of the array, it's only balanced if there are no open parentheses
        currOpen == 0
      } else if (chars(currIdx) == '(') {
        // Add to the number of open parentheses count
        balanceHelper(currOpen + 1, currIdx + 1)
      } else if (chars(currIdx) == ')') {
        if (currOpen <= 0) {
          return false
        }
        // Subtract from the number of open parentheses
        balanceHelper(currOpen - 1, currIdx + 1)
      } else {
        // The current character is not a parenthesis, so just continue
        balanceHelper(currOpen, currIdx + 1)
      }
    }

    balanceHelper(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    @tailrec
    def traverse(idx: Int, until: Int, unmatchedLeft: Int, unmatchedRight: Int):  (Int, Int) = {
      if (idx >= until) {
        return (unmatchedLeft, unmatchedRight)
      }
      chars(idx) match {
        case '(' => traverse(idx+1, until, unmatchedLeft + 1, unmatchedRight)
        case ')' => {
          if (unmatchedLeft > 0) {
            traverse(idx+1, until, unmatchedLeft - 1, unmatchedRight)
          } else {
            traverse(idx + 1, until, unmatchedLeft, unmatchedRight + 1)
          }
        }
        case _ => traverse(idx + 1, until, unmatchedLeft, unmatchedRight)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = from + ((until - from) / 2)
        val ((uL1, uR1), (uL2, uR2)) = parallel(reduce(from, mid), reduce(mid, until))
        if (uL1 > uR2) {
          (uL1 - uR2 + uL2, uR1)
        } else {
          (uL2, uR1 + uR2 - uL1)
        }
      }
    }

    reduce(0, chars.length) == (0, 0)

  // For those who want more:
  // Prove that your reduction operator is associative!


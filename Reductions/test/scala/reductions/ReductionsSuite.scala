package reductions

import java.util.concurrent.*
import scala.collection.*
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

class ReductionsSuite extends munit.FunSuite:
  /*****************
   * LINE OF SIGHT *
   *****************/

  import LineOfSight.*
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assertEquals(output.toList, List(0f, 1f, 4f, 4f))
  }




  /*******************************
   * PARALLEL COUNT CHANGE SUITE *
   *******************************/

  import ParallelCountChange.*

  test("countChange should return 0 for money < 0") {
    def check(money: Int, coins: List[Int]) =
      assert(countChange(money, coins) == 0,
        s"countChange($money, _) should be 0")

    check(-1, List())
    check(-1, List(1, 2, 3))
    check(-Int.MinValue, List())
    check(-Int.MinValue, List(1, 2, 3))
  }

  test("countChange should return 1 when money == 0") {
    def check(coins: List[Int]) =
      assert(countChange(0, coins) == 1,
        s"countChange(0, _) should be 1")

    check(List())
    check(List(1, 2, 3))
    check(List.range(1, 100))
  }

  test("countChange should return 0 for money > 0 and coins = List()") {
    def check(money: Int) =
      assert(countChange(money, List()) == 0,
        s"countChange($money, List()) should be 0")

    check(1)
    check(Int.MaxValue)
  }

  test("countChange should work when there is only one coin") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")

    check(1, List(1), 1)
    check(2, List(1), 1)
    check(1, List(2), 0)
    check(Int.MaxValue, List(Int.MaxValue), 1)
    check(Int.MaxValue - 1, List(Int.MaxValue), 0)
  }

  test("countChange should work for multi-coins") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")

    check(50, List(1, 2, 5, 10), 341)
    check(250, List(1, 2, 5, 10, 20, 50), 177863)
  }

  test("parCountChange should return 0 for money < 0") {
    def check(money: Int, coins: List[Int]): Unit =
      assert(parCountChange(money, coins, combinedThreshold(money, coins)) == 0,
        s"parCountChange($money, _) should be 0")

    check(-1, List())
    check(-1, List(1, 2, 3))
    check(-Int.MinValue, List())
    check(-Int.MinValue, List(1, 2, 3))
  }

  test("parCountChange should return 1 when money == 0") {
    def check(coins: List[Int]): Unit =
      assert(parCountChange(0, coins, combinedThreshold(0, coins)) == 1,
        s"parCountChange(0, _) should be 1")

    check(List())
    check(List(1, 2, 3))
    check(List.range(1, 100))
  }

  test("parCountChange should return 0 for money > 0 and coins = List()") {
    def check(money: Int): Unit =
      assert(parCountChange(money, List(), combinedThreshold(money, List())) == 0,
        s"parCountChange($money, List()) should be 0")

    check(1)
    check(Int.MaxValue)
  }

  test("parCountChange should work when there is only one coin") {
    def check(money: Int, coins: List[Int], expected: Int): Unit =
      assert(parCountChange(money, coins, combinedThreshold(money, coins)) == expected,
        s"parCountChange($money, $coins) should be $expected")

    check(1, List(1), 1)
    check(2, List(1), 1)
    check(1, List(2), 0)
    check(Int.MaxValue, List(Int.MaxValue), 1)
    check(Int.MaxValue - 1, List(Int.MaxValue), 0)
  }

  test("parCountChange should work for multi-coins") {
    def check(money: Int, coins: List[Int], expected: Int): Unit =
      assert(parCountChange(money, coins, combinedThreshold(money, coins)) == expected,
        s"parCountChange($money, $coins) should be $expected")

    check(50, List(1, 2, 5, 10), 341)
    check(250, List(1, 2, 5, 10, 20, 50), 177863)
  }


  /**********************************
   * PARALLEL PARENTHESES BALANCING *
   **********************************/

  import ParallelParenthesesBalancing.*

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("parBalance should work for empty string") {
    def check(input: String, expected: Boolean): Unit =
      assert(parBalance(input.toArray, 1) == expected,
        s"parBalance($input) should be $expected")

    check("", true)
  }

  test("parBalance should work for string of length 1") {
    def check(input: String, expected: Boolean): Unit =
      assert(parBalance(input.toArray, 1) == expected,
        s"parBalance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("parBalance should work for string of length 2") {
    def check(input: String, expected: Boolean): Unit =
      assert(parBalance(input.toArray, 1) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("parBalance should work for longer string") {
    assert(!parBalance(")(t(_)l)".toArray, 1))
    assert(parBalance("()fg(((0)lq))".toArray, 1))
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds


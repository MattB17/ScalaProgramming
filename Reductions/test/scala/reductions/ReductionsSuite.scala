package reductions

import reductions.Tree.*

import java.util.concurrent.*
import scala.collection.*
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

class ReductionsSuite extends munit.FunSuite:
  /*****************
   * LINE OF SIGHT *
   *****************/

  import LineOfSight.*

  test("lineOfSight on 1 element array") {
    val output = new Array[Float](1)
    lineOfSight(Array[Float](3f), output)
    assertEquals(output.toList, List(0f))
  }

  test("lineOfSight on 2 element array") {
    val output = new Array[Float](2)
    lineOfSight(Array[Float](0f, 3f), output)
    assertEquals(output.toList, List(0f, 3f))
  }

  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assertEquals(output.toList, List(0f, 1f, 4f, 4f))
  }

  test("lineOfSight on changing terrain") {
    val output = new Array[Float](10)
    lineOfSight(Array[Float](0f, 1f, 4f, 5f, 3f, 15f, 15f, 7f, 9f, 54f), output)
    assertEquals(output.toList, List(0f, 1f, 2f, 2f, 2f, 3f, 3f, 3f, 3f, 6f))
  }

  test("upsweepSequential with 1 element array") {
    assertEquals(upsweepSequential(Array[Float](3f), 0, 1), 0f)
  }

  test("upsweepSequential on 2 element array") {
    assertEquals(upsweepSequential(Array[Float](0f, 3f), 0, 2), 3f)
  }

  test("upsweepSequential on subset of the array") {
    assertEquals(upsweepSequential(Array[Float](0f, 1f, 4f, 5f, 3f, 15f, 15f, 7f, 9f, 54f), 1, 6), 3f)
  }

  test("upsweep on 1 element array") {
    val input = Array[Float](3f)
    val result = upsweep(input, 0, 1, 2)
    assertEquals(result, Leaf(0, 1, 0f))
  }

  test("upsweep on 2 element array") {
    val input = Array[Float](0f, 3f)
    val result = upsweep(input, 0, 2, 2)
    assertEquals(result, Node(Leaf(0, 1, 0f), Leaf(1, 2, 3f)))
  }

  test("upsweep on 4 element array") {
    val input = Array[Float](0f, 1f, 8f, 9f)
    val t0 = Node(Leaf(0, 1, 0f), Leaf(1, 2, 1f))
    val t1 = Node(Leaf(2, 3, 4f), Leaf(3, 4, 3f))
    val result = upsweep(input, 0, 4, 2)
    assertEquals(result, Node(t0, t1))
  }

  test("upsweep on 10 element array") {
    val input = Array[Float](0f, 1f, 4f, 6f, 4f, 15f, 12f, 7f, 8f, 54f)
    val t0 = Node(Leaf(0, 1, 0f), Leaf(1, 2, 1f))
    val t1 = Node(Leaf(3, 4, 2f), Leaf(4, 5, 1f))
    val t2 = Node(Leaf(2, 3, 2f), t1)
    val t3 = Node(Leaf(5, 6, 3f), Leaf(6, 7, 2f))
    val t4 = Node(Leaf(8, 9, 1f), Leaf(9, 10, 6f))
    val t5 = Node(Leaf(7, 8, 1f), t4)
    val t6 = Node(t0, t2)
    val t7 = Node(t3, t5)
    val result = upsweep(input, 0, 10, 2)
    assertEquals(result, Node(t6, t7))
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


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

  test("upsweepSequential on 2 element array") {
    assertEquals(upsweepSequential(Array[Float](0f, 3f), 1, 2), 3f)
  }

  test("upsweepSequential on subset of the array") {
    assertEquals(upsweepSequential(Array[Float](0f, 1f, 4f, 5f, 3f, 15f, 15f, 7f, 9f, 54f), 1, 6), 3f)
  }

  test("upsweep on 2 element array") {
    val input = Array[Float](0f, 3f)
    val result = upsweep(input, 1, 2, 1)
    assertEquals(result, Leaf(1, 2, 3f))
  }

  test("upsweep on 3 element array") {
    val input = Array[Float](0f, 0f, 6f)
    val result = upsweep(input, 1, 3, 1)
    assertEquals(result, Node(Leaf(1, 2, 0f), Leaf(2, 3, 3f)))
  }

  test("upsweep on 5 element array") {
    val input = Array[Float](0f, 0f, 2f, 12f, 12f)
    val t0 = Node(Leaf(1, 2, 0f), Leaf(2, 3, 1f))
    val t1 = Node(Leaf(3, 4, 4f), Leaf(4, 5, 3f))
    val result = upsweep(input, 1, 5, 1)
    assertEquals(result, Node(t0, t1))
  }

  test("upsweep on 11 element array") {
    val input = Array[Float](0f, 0f, 2f, 6f, 8f, 5f, 18f, 14f, 8f, 9f, 60f)
    val t0 = Node(Leaf(1, 2, 0f), Leaf(2, 3, 1f))
    val t1 = Node(Leaf(4, 5, 2f), Leaf(5, 6, 1f))
    val t2 = Node(Leaf(3, 4, 2f), t1)
    val t3 = Node(Leaf(6, 7, 3f), Leaf(7, 8, 2f))
    val t4 = Node(Leaf(9, 10, 1f), Leaf(10, 11, 6f))
    val t5 = Node(Leaf(8, 9, 1f), t4)
    val t6 = Node(t0, t2)
    val t7 = Node(t3, t5)
    val result = upsweep(input, 1, 11, 1)
    assertEquals(result, Node(t6, t7))
  }

  test("downsweepSequential on 2 element array") {
    val output = new Array[Float](2)
    val input = Array[Float](0f, 3f)
    downsweepSequential(input, output, 2f, 1, 2)
    assertEquals(output.toList, List(0f, 3f))
  }

  test("downsweepSequential on 3 element array") {
    val output = new Array[Float](3)
    val input = Array[Float](0f, 0f, 6f)
    downsweepSequential(input, output, 0f, 1, 3)
    assertEquals(output.toList, List(0f, 0f, 3f))
  }

  test("downsweepSequential on subset of 4 element array") {
    val output = new Array[Float](4)
    val input = Array[Float](0f, 1f, 8f, 9f)
    downsweepSequential(input, output, 5f, 1, 3)
    assertEquals(output.toList, List(0f, 5f, 5f, 0f))
  }

  test("downsweepSequential on subset of 10 element array") {
    val output = new Array[Float](10)
    val input = Array[Float](0f, 1f, 4f, 6f, 4f, 25f, 12f, 7f, 8f, 54f)
    downsweepSequential(input, output, 4f, 3, 7)
    assertEquals(output.toList, List(0f, 0f, 0f, 4f, 4f, 5f, 5f, 0f, 0f, 0f))
  }

  test("downsweep on leaf") {
    val output = new Array[Float](2)
    val input = Array[Float](0f, 3f)
    val t = Leaf(1, 2, 3f)
    downsweep(input, output, 0f, t)
    assertEquals(output.toList, List(0f, 3f))
  }

  test("downsweep on 1 level tree") {
    val output = new Array[Float](3)
    val input = Array[Float](0f, 0f, 6f)
    val t = Node(Leaf(1, 2, 0f), Leaf(2, 3, 3f))
    downsweep(input, output, 0f, t)
    assertEquals(output.toList, List(0f, 0f, 3f))
  }

  test("downsweep on 2 level tree") {
    val output = new Array[Float](5)
    val input = Array[Float](0f, 0f, 2f, 12f, 12f)
    val t0 = Node(Leaf(1, 2, 0f), Leaf(2, 3, 1f))
    val t1 = Node(Leaf(3, 4, 4f), Leaf(4, 5, 3f))
    val t = Node(t0, t1)
    downsweep(input, output, 0f, t)
    assertEquals(output.toList, List(0f, 0f, 1f, 4f, 4f))
  }

  test("downsweep on complex tree") {
    val output = new Array[Float](11)
    val input = Array[Float](0f, 0f, 2f, 6f, 8f, 5f, 18f, 14f, 8f, 9f, 60f)
    val t0 = Node(Leaf(1, 2, 0f), Leaf(2, 3, 1f))
    val t1 = Node(Leaf(4, 5, 2f), Leaf(5, 6, 1f))
    val t2 = Node(Leaf(3, 4, 2f), t1)
    val t3 = Node(Leaf(6, 7, 3f), Leaf(7, 8, 2f))
    val t4 = Node(Leaf(9, 10, 1f), Leaf(10, 11, 6f))
    val t5 = Node(Leaf(8, 9, 1f), t4)
    val t6 = Node(t0, t2)
    val t7 = Node(t3, t5)
    val t = Node(t6, t7)
    downsweep(input, output, 0f, t)
    assertEquals(output.toList, List(0f, 0f, 1f, 2f, 2f, 2f, 3f, 3f, 3f, 3f, 6f))
  }

  test("parLineOfSight on 1 element array") {
    val output = new Array[Float](1)
    parLineOfSight(Array[Float](3f), output, 1)
    assertEquals(output.toList, List(0f))
  }

  test("parLineOfSight on 2 element array") {
    val output = new Array[Float](2)
    parLineOfSight(Array[Float](0f, 3f), output, 1)
    assertEquals(output.toList, List(0f, 3f))
  }

  test("parLineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f), output, 1)
    assertEquals(output.toList, List(0f, 1f, 4f, 4f))
  }

  test("parLineOfSight on changing terrain") {
    val output = new Array[Float](10)
    parLineOfSight(Array[Float](0f, 1f, 4f, 5f, 3f, 15f, 15f, 7f, 9f, 54f), output, 1)
    assertEquals(output.toList, List(0f, 1f, 2f, 2f, 2f, 3f, 3f, 3f, 3f, 6f))
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


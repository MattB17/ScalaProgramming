package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite {

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
    assert(!contains((x: Int) => x < 0, 10))
    assert(contains((x: Int) => x < 0, -5))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSingletons {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSingletons {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton of 1 contains 1")
      assert(!contains(s1, 2), "Singleton of 1 does not contain 2")
      assert(!contains(s1, 0), "Singleton of 1 does not contain 0")
    }
  }

  test("union contains all elements of each set") {
    new TestSingletons {
      val s: FunSet = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  trait TestRangeSets {
    val s = (elem: Int) => elem <= 3
    val t = (elem: Int) => elem >= 2
  }

  test("intersection contains only elements in both sets") {
    new TestRangeSets {
      val intersection: FunSet = intersect(s, t)
      assert(contains(s, 1) && !contains(t, 1) && !contains(intersection, 1),
        "Intersection: In s but not t")
      assert(contains(s, 2) && contains(t, 2) && contains(intersection, 2),
        "Intersection: In both s and t - test 1")
      assert(contains(s, 3) && contains(t, 3) && contains(intersection, 3),
        "Intersection: In both s and t - test 2")
      assert(!contains(s, 4) && contains(t, 4) && !contains(intersection, 4),
        "Intersection: In t but not s")
    }
  }

  test("diff contains elements in one set but not the other") {
    new TestRangeSets {
      val d: FunSet = diff(s, t)
      assert(contains(s, 1) && !contains(t, 1) && contains(d, 1),
             "Diff: In s but not t")
      assert(contains(s, 2) && contains(t, 2) && !contains(d, 2),
             "Diff: In both s and t - test 1")
      assert(contains(s, 3) && contains(t, 3) && !contains(d, 3),
             "Diff: In both s and t - test 2")
      assert(!contains(s, 4) && contains(t, 4) && !contains(d, 4),
             "Diff: In t but not s")
    }
  }

  test("filter") {
    val s = (x: Int) => x >= 0
    val p = (x: Int) => x % 2 == 0
    val f: FunSet = filter(s, p)
    assert(!contains(f, -2), "Filter on -2")
    assert(!contains(f, -1), "Filter on -1")
    assert(contains(f, 0), "Filter on 0")
    assert(!contains(f, 1), "Filter on 1")
    assert(contains(f, 2), "Filter on 2")
  }



  import scala.concurrent.duration._
  override val munitTimeout = 10.seconds
}

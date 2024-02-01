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

  test("forall on range set is true") {
    new TestRangeSets {
      val p = (x: Int) => x < 5
      assert(forall(s, p), "forall on range set is true")
    }
  }

  test("forall fails on last element of set") {
    new TestRangeSets {
      val p = (x: Int) => x <= 1
      assert(!forall(s, p), "forall fails on last element of set")
    }
  }

  test("forall fails on multiple elements of set") {
    new TestRangeSets {
      val p = (x: Int) => x % 2 == 0
      assert(!forall(s, p), "forall fails on multiple elements of set")
    }
  }

  test("forall is true on smaller range") {
    val s = (x: Int) => x > -20 && x < -10
    val p = (x: Int) => x >= -30 && x < 0
    assert(forall(s, p), "forall is true on smaller range")
  }

  test("exists is true on all elements") {
    val s = (x: Int) => x >= 10 && x <= 20
    val p = (x: Int) => x > 0
    assert(exists(s, p), "exists is true on all elements")
  }

  test("exists is true for multiple elements") {
    new TestRangeSets {
      val p = (x: Int) => x % 2 == 1
      assert(exists(s, p), "exists is true for multiple elements")
    }
  }

  test("exists is true for only one element") {
    val s = (x: Int) => x <= 10 || x > 20
    val p = (x: Int) => x >= 10 && x <= 20
    assert(exists(s, p), "exists is true for only one element")
  }

  test("exists is false") {
    new TestRangeSets {
      val p = (x: Int) => x >= 10
      assert(!exists(s, p), "exists is false")
    }
  }

  test("map negation") {
    new TestRangeSets {
      val f = (y: Int) => -y
      val mapped_s = map(s, f)
      assert(contains(mapped_s, 1000), "-1000 <= 3 so 1000 in mapped_s")
      assert(contains(mapped_s, 10), "-10 <= 3 so 10 in mapped_s")
      assert(contains(mapped_s, -3), "3 <= 3 so -3 in mapped_s")
      assert(!contains(mapped_s, -10), "10 > 3 so -10 not in mapped_s")
      assert(!contains(mapped_s, -1000), "1000 > 3 so -1000 not in mapped_s")
    }
  }

  test("map square") {
    new TestRangeSets {
      val f = (y: Int) => y * y
      val mapped_t = map(t, f)
      assert(!contains(mapped_t, 1), "1 not in t so 1 not in mapped_t")
      assert(!contains(mapped_t, 3), "sqrt(3) not in t so 3 not in mapped_t")
      assert(contains(mapped_t, 4), "2 in t so 4 in mapped_t")
      assert(!contains(mapped_t, 7), "sqrt(7) not in t so 7 not in mapped_t")
      assert(contains(mapped_t, 9), "3 in t so 9 in mapped_t")
    }
  }

  test("map to singleton") {
    new TestRangeSets {
      val f = (y: Int) => 1
      val mapped_s = map(s, f)
      assert(!contains(mapped_s, -5), "-5 not in mapped_s")
      assert(contains(mapped_s, 1), "1 in mapped_s")
      assert(!contains(mapped_s, 7), "7 not in mapped_s")
    }
  }


  import scala.concurrent.duration._
  override val munitTimeout = 10.seconds
}

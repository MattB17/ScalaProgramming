package kmeans

import java.util.concurrent.*
import scala.collection.{mutable, Map, Seq}
import scala.collection.parallel.{ParMap, ParSeq}
import scala.collection.parallel.CollectionConverters.*
import scala.math.*

class KMeansSuite extends munit.FunSuite:
  object KM extends KMeans
  import KM.*

  def checkParClassify(points: ParSeq[Point], means: ParSeq[Point], expected: ParMap[Point, ParSeq[Point]]): Unit =
    assertEquals(classify(points, means), expected, s"classify($points, $means) should equal to $expected")

  def pointsEqual(a: Point, b: Point): Boolean = {
    a.x == b.x && a.y == b.y && a.z == b.z
  }

  test("'classify' should work for empty 'points' and empty 'means'") {
    val points: ParSeq[Point] = IndexedSeq().par
    val means: ParSeq[Point] = IndexedSeq().par
    val expected = ParMap[Point, ParSeq[Point]]()
    checkParClassify(points, means, expected)
  }

  test("classify with no points and one mean") {
    val m0: Point = Point(1, 1, 1)
    val points: ParSeq[Point] = IndexedSeq().par
    val means: ParSeq[Point] = IndexedSeq(m0).par
    val expected = ParMap[Point, ParSeq[Point]](m0 -> IndexedSeq().par)
    checkParClassify(points, means, expected)
  }

  test("classify with multiple points and only one mean") {
    val p0: Point = Point(0, 0, 0)
    val p1: Point = Point(1, 1, 1)
    val p2: Point = Point(2, 2, 2)
    val m0: Point = Point(1, 1, 1)
    val points: ParSeq[Point] = IndexedSeq(p0, p1, p2).par
    val means: ParSeq[Point] = IndexedSeq(m0).par
    val expected = ParMap[Point, ParSeq[Point]](m0 -> IndexedSeq(p0, p1, p2).par)
    checkParClassify(points, means, expected)
  }

  test("classify with no points and multiple means") {
    val m0: Point = Point(1, 1, 1)
    val m1: Point = Point(100, 100, 100)
    val points: ParSeq[Point] = IndexedSeq().par
    val means: ParSeq[Point] = IndexedSeq(m0, m1).par
    val expected = ParMap[Point, ParSeq[Point]](m0 -> IndexedSeq().par, m1 -> IndexedSeq().par)
    checkParClassify(points, means, expected)
  }

  test("classify multiple points and multiple means") {
    val p0: Point = Point(0, 0, 0)
    val p1: Point = Point(1, 1, 1)
    val p2: Point = Point(2, 2, 2)
    val m0: Point = Point(1, 1, 1)
    val m1: Point = Point(100, 100, 100)
    val points: ParSeq[Point] = IndexedSeq(p0, p1, p2).par
    val means: ParSeq[Point] = IndexedSeq(m0, m1).par
    val expected = ParMap[Point, ParSeq[Point]](
      m0 -> IndexedSeq(p0, p1, p2).par,
      m1 -> IndexedSeq().par
    )
    checkParClassify(points, means, expected)
  }

  test("'update' should work for empty 'classified' and empty 'oldMeans'") {
    val oldMeans: ParSeq[Point] = IndexedSeq().par
    val classified = ParMap[Point, ParSeq[Point]]()
    val expected = ParSeq[Point]()
    assertEquals(update(classified, oldMeans), expected)
  }

  test("update with no classified and one mean") {
    val m0: Point = Point(1, 1, 1)
    val oldMeans: ParSeq[Point] = IndexedSeq(m0).par
    val classified = ParMap[Point, ParSeq[Point]](m0 -> IndexedSeq().par)
    val expected: ParSeq[Point] = IndexedSeq(m0).par
    assertEquals(update(classified, oldMeans), expected)
  }

  test("update with multiple points and only one mean") {
    val p0: Point = Point(0, 0, 0)
    val p1: Point = Point(1, 1, 1)
    val p2: Point = Point(2, 2, 2)
    val m0: Point = Point(1, 1, 1)
    val oldMeans: ParSeq[Point] = IndexedSeq(m0).par
    val classified = ParMap[Point, ParSeq[Point]](m0 -> IndexedSeq(p0, p1, p2).par)
    val result = update(classified, oldMeans)
    assert(result.length == 1)
    assert(pointsEqual(result.head, m0))
  }

  test("update multiple points and multiple means") {
    val p0: Point = Point(0, 0, 0)
    val p1: Point = Point(1, 1, 1)
    val p2: Point = Point(2, 2, 2)
    val m0: Point = Point(1, 1, 1)
    val m1: Point = Point(100, 100, 100)
    val oldMeans: ParSeq[Point] = IndexedSeq(m0, m1).par
    val classified = ParMap[Point, ParSeq[Point]](
      m0 -> IndexedSeq(p0, p1, p2).par,
      m1 -> IndexedSeq().par
    )
    val result = update(classified, oldMeans)
    assert(result.length == 2)
    assert(pointsEqual(result.head, m0))
    assert(pointsEqual(result.tail.head, m1))
  }

  test("update each mean has 1 point in cluster") {
    val p0: Point = Point(0, 0, 0)
    val p1: Point = Point(99,99,99)
    val p2: Point = Point(-99, -99, -99)
    val m0: Point = Point(1, 1, 1)
    val m1: Point = Point(100, 100, 100)
    val m2: Point = Point(-100, -100, -100)
    val oldMeans: ParSeq[Point] = IndexedSeq(m0, m1, m2).par
    val classified = ParMap[Point, ParSeq[Point]](
      m0 -> IndexedSeq(p0).par,
      m1 -> IndexedSeq(p1).par,
      m2 -> IndexedSeq(p2).par
    )
    val result = update(classified, oldMeans)
    assert(result.length == 3)
    assert(pointsEqual(result.head, p0))
    assert(pointsEqual(result.tail.head, p1))
    assert(pointsEqual(result.tail.tail.head, p2))
  }

  test("converged with empty means") {
    val oldMeans: ParSeq[Point] = IndexedSeq().par
    val newMeans: ParSeq[Point] = IndexedSeq().par
    assert(converged(1, oldMeans, newMeans))
  }

  test("converged true with 1 mean") {
    val oldMeans: ParSeq[Point] = IndexedSeq(Point(1, 1, 1)).par
    val newMeans: ParSeq[Point] = IndexedSeq(Point(2, 1, 1)).par
    assert(converged(1, oldMeans, newMeans))
  }

  test("converged false with 1 mean") {
    val oldMeans: ParSeq[Point] = IndexedSeq(Point(1, 1, 1)).par
    val newMeans: ParSeq[Point] = IndexedSeq(Point(3, 1, 1)).par
    assert(!converged(3, oldMeans, newMeans))
  }

  test("converged true with multiple means") {
    val m0: Point = Point(0, 0, 0)
    val m1: Point = Point(10, 10, 10)
    val m2: Point = Point(20, 20, 20)
    val m3: Point = Point(1, 1, 1)
    val m4: Point = Point(9, 9, 9)
    val m5: Point = Point(20, 21, 21)
    val oldMeans: ParSeq[Point] = IndexedSeq(m0, m1, m2).par
    val newMeans: ParSeq[Point] = IndexedSeq(m3, m4, m5).par
    assert(converged(3, oldMeans, newMeans))
  }

  test("converged false with multiple means") {
    val m0: Point = Point(-10, -10, -10)
    val m1: Point = Point(0, 0, 0)
    val m2: Point = Point(10, 10, 10)
    val m3: Point = Point(-10, -10, -10)
    val m4: Point = Point(0, 0, 1)
    val m5: Point = Point(8, 10, 10)
    val oldMeans: ParSeq[Point] = IndexedSeq(m0, m1, m2).par
    val newMeans: ParSeq[Point] = IndexedSeq(m3, m4, m5).par
    assert(!converged(3, oldMeans, newMeans))
  }

  test("converged true with eta of 0") {
    val m0: Point = Point(-1, -1, -1)
    val m1: Point = Point(0, 0, 0)
    val m2: Point = Point(1, 1, 1)
    val oldMeans: ParSeq[Point] = IndexedSeq(m0, m1, m2).par
    val newMeans: ParSeq[Point] = IndexedSeq(m0, m1, m2).par
    assert(converged(0, oldMeans, newMeans))
  }

  test("converged false with eta of 0") {
    val m0: Point = Point(-1, -1, -1)
    val m1: Point = Point(0, 0, 0)
    val m2: Point = Point(1, 1, 1)
    val m3: Point = Point(1, 1, 2)
    val oldMeans: ParSeq[Point] = IndexedSeq(m0, m1, m2).par
    val newMeans: ParSeq[Point] = IndexedSeq(m0, m1, m3).par
    assert(!converged(0, oldMeans, newMeans))
  }

  test("kMeans on empty means and empty points") {
    val points: ParSeq[Point] = IndexedSeq().par
    val means: ParSeq[Point] = IndexedSeq().par
    assertEquals(kMeans(points, means, 1), IndexedSeq().par)
  }

  test("kMeans with single mean that has already converged") {
    val p0: Point = Point(-1, -1, -1)
    val p1: Point = Point(0, 0, 0)
    val p2: Point = Point(1, 1, 1)
    val m0: Point = Point(0, 0, 0)
    val points: ParSeq[Point] = IndexedSeq(p0, p1, p2).par
    val means: ParSeq[Point] = IndexedSeq(m0).par
    val result = kMeans(points, means, 1)
    assertEquals(result.length, 1)
    assert(pointsEqual(result.head, m0))
  }

  test("kMeans with single mean that has not converged") {
    val p0: Point = Point(-1, -1, -1)
    val p1: Point = Point(0, 0, 0)
    val p2: Point = Point(1, 1, 1)
    val m0: Point = Point(-1, -1, -1)
    val points: ParSeq[Point] = IndexedSeq(p0, p1, p2).par
    val means: ParSeq[Point] = IndexedSeq(m0).par
    val result = kMeans(points, means, 2)
    assertEquals(result.length, 1)
    assert(pointsEqual(result.head, p1))
  }

  test("kkMeans multiple means that have converged") {
    val p0: Point = Point(-1, -1, -1)
    val p1: Point = Point(0, 0, 0)
    val p2: Point = Point(1, 1, 1)
    val p3: Point = Point(99, 99, 99)
    val p4: Point = Point(100, 100, 100)
    val p5: Point = Point(101, 101, 101)
    val m0: Point = Point(0, 0, 0)
    val m1: Point = Point(100, 100, 100)
    val points: ParSeq[Point] = IndexedSeq(p0, p1, p2, p3, p4, p5).par
    val means: ParSeq[Point] = IndexedSeq(m0, m1).par
    val result = kMeans(points, means, 2)
    assertEquals(result.length, 2)
    assert(pointsEqual(result.head, p1))
    assert(pointsEqual(result.tail.head, p4))
  }

  test("kkMeans multiple means one has not converged") {
    val p0: Point = Point(-1, -1, -1)
    val p1: Point = Point(0, 0, 0)
    val p2: Point = Point(1, 1, 1)
    val p3: Point = Point(99, 99, 99)
    val p4: Point = Point(100, 100, 100)
    val p5: Point = Point(101, 101, 101)
    val m0: Point = Point(-1, -1, 0)
    val m1: Point = Point(100, 100, 100)
    val points: ParSeq[Point] = IndexedSeq(p0, p1, p2, p3, p4, p5).par
    val means: ParSeq[Point] = IndexedSeq(m0, m1).par
    val result = kMeans(points, means, 2)
    assertEquals(result.length, 2)
    assert(pointsEqual(result.head, p1))
    assert(pointsEqual(result.tail.head, p4))
  }

  test("kkMeans multiple means, none have converged") {
    val p0: Point = Point(-1, -1, -1)
    val p1: Point = Point(0, 0, 0)
    val p2: Point = Point(1, 1, 1)
    val p3: Point = Point(99, 99, 99)
    val p4: Point = Point(100, 100, 100)
    val p5: Point = Point(101, 101, 101)
    val m0: Point = Point(-1, -1, 0)
    val m1: Point = Point(98, 97, 98)
    val points: ParSeq[Point] = IndexedSeq(p0, p1, p2, p3, p4, p5).par
    val means: ParSeq[Point] = IndexedSeq(m0, m1).par
    val result = kMeans(points, means, 2)
    assertEquals(result.length, 2)
    assert(pointsEqual(result.head, p1))
    assert(pointsEqual(result.tail.head, p4))
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds



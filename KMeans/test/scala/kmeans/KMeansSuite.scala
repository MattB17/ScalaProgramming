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

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds



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

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds



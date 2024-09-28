package observatory

import Visualization.*
import com.sksamuel.scrimage.pixels.Pixel

trait VisualizationTest extends MilestoneSuite:
  private val milestoneTest = namedMilestoneTest("raw data display", 2) _

  test("greatCircleDistanceAngle with same location") {
    val loc = Location(12.5, 13.8)
    assertEquals(greatCircleDistanceAngle(loc, loc), 0.0)
  }

  test("greatCircleDistanceAngle with antipodes and negative longitude") {
    val loc0 = Location(10, -5)
    val loc1 = Location(-10, 175)
    assertEquals(greatCircleDistanceAngle(loc0, loc1), math.Pi)
  }

  test("greatCircleDistanceAngle with antipodes and positive longitude") {
    val loc0 = Location(25, 34)
    val loc1 = Location(-25, -146)
    assertEquals(greatCircleDistanceAngle(loc0, loc1), math.Pi)
  }

  test("greatCircleDistanceAngle else clause") {
    val loc0 = Location(10, 10)
    val loc1 = Location(20, 120)
    assertEqualsDouble(greatCircleDistanceAngle(loc0, loc1), 1.830835, 0.0001)
  }

  test("predictTemperature list has length 1 should just give the temperature") {
    val loc0 = Location(10, 10)
    val loc1 = Location(20, 100)
    val loc2 = Location(10.1, 10.0)
    val temps0: Iterable[(Location, Temperature)] = IndexedSeq((loc1, 20))
    assertEqualsDouble(predictTemperature(temps0, loc0), 20.0, 0.001)

    val temps1: Iterable[(Location, Temperature)] = IndexedSeq((loc2, -5))
    assertEqualsDouble(predictTemperature(temps1, loc0), -5.0, 0.001)
  }

  test("predictTemperature should give temp of close point") {
    val loc = Location(10, 10)
    val temps: Iterable[(Location, Temperature)] = IndexedSeq(
      (Location(-20, -10), -10.5),
      (Location(0, 0), 45),
      (loc, 30))
    assertEqualsDouble(predictTemperature(temps, loc), 30.0, 0.001)
  }

  test("predictTemperature with no close temperature") {
    val loc = Location(10, 10)
    val temps: Iterable[(Location, Temperature)] = IndexedSeq(
      (Location(20, 120), 20),
      (Location(-10, -170), 5),
      (Location(50, 50), 26))
    assertEqualsDouble(predictTemperature(temps, loc), 20.943003, 0.001)
  }

  test("getLinearInterpolation predicts left point") {
    assertEquals(getLinearInterpolation(4, 4, 110, 10.5, 30), 110)
  }

  test("getLinearInterpolation predicts right point") {
    assertEquals(getLinearInterpolation(5, 1, 45, 5, 60), 60)
  }

  test("getLinearInterpolation between two points") {
    assertEquals(getLinearInterpolation(3, 1, 20, 5, 10), 15)
    assertEquals(getLinearInterpolation(3.5, 1, 5, 5, 25), 18)
  }

  test("interpolateColor only one color with same temperature") {
    val col = Color(200, 125, 130)
    val points: Iterable[(Temperature, Color)] = IndexedSeq((20, col))
    assertEquals(interpolateColor(points, 20), col)
  }

  test("interpolateColor only one color with greater temperature") {
    val col = Color(150, 0, 30)
    val points: Iterable[(Temperature, Color)] = IndexedSeq((20, col))
    assertEquals(interpolateColor(points, 5), col)
  }

  test("interpolateColor multiple colors with greater temperature") {
    val col = Color(125, 101, 130)
    val points: Iterable[(Temperature, Color)] = IndexedSeq(
      (5, col),
      (20, Color(200, 200, 200)))
    assertEquals(interpolateColor(points, 3), col)
  }

  test("interpolateColor only one color with smaller temperature") {
    val col = Color(255, 255, 255)
    val points: Iterable[(Temperature, Color)] = IndexedSeq((-10, col))
    assertEquals(interpolateColor(points, 5), col)
  }

  test("interpolateColor multiple colors with smaller temperature") {
    val col = Color(111, 25, 48)
    val points: Iterable[(Temperature, Color)] = IndexedSeq(
      (-1, Color(50, 50, 50)),
      (1, col),
      (-0.5, Color(75, 75, 75)))
    assertEquals(interpolateColor(points, 3), col)
  }

  test("interpolateColor with one color above and below") {
    val points: Iterable[(Temperature, Color)] = IndexedSeq(
      (5, Color(0, 0, 0)),
      (1, Color(255, 0, 255)))
    assertEquals(interpolateColor(points, 3), Color(128, 0, 128))
  }

  test("interpolateColor multiple colors above and below") {
    val points: Iterable[(Temperature, Color)] = IndexedSeq(
      (-1, Color(50, 50, 50)),
      (-0.5, Color(75, 75, 75)),
      (1, Color(99, 99, 99)),
      (5, Color(101, 101, 101)),
      (15, Color(150, 150, 150)))
    assertEquals(interpolateColor(points, 3), Color(100, 100, 100))
  }

  test("computePixel one temp and one color") {
    val temps: Iterable[(Location, Temperature)] = IndexedSeq(
      (Location(10, 10), 20))
    val colors: Iterable[(Temperature, Color)] = IndexedSeq((20, Color(100, 100, 100)))

    val expectedPixel = Pixel(180, 90, 100, 100, 100, 255)
    assertEquals(computePixel(180, 90, temps, colors), expectedPixel)
  }

  test("computePixel multiple temps and one color") {
    val temps: Iterable[(Location, Temperature)] = IndexedSeq(
      (Location(20, 120), 20),
      (Location(-10, -170), 5),
      (Location(50, 50), 26))
    val colors: Iterable[(Temperature, Color)] = IndexedSeq((5, Color(94, 123, 240)))

    val expectedPixel = Pixel(120, 0, 94, 123, 240, 255)
    assertEquals(computePixel(120, 0, temps, colors), expectedPixel)
  }

  test("computePixel one temp and multiple colors") {
    val temps: Iterable[(Location, Temperature)] = IndexedSeq((Location(20, 100), 3))
    val colors: Iterable[(Temperature, Color)] = IndexedSeq(
      (-1, Color(50, 50, 50)),
      (-0.5, Color(75, 75, 75)),
      (1, Color(99, 99, 99)),
      (5, Color(101, 101, 101)),
      (15, Color(150, 150, 150)))

    val expectedPixel = Pixel(20, 20, 100, 100, 100, 255)
    assertEquals(computePixel(20, 20, temps, colors), expectedPixel)
  }

  test("computePixel multiple temps and multiple colors") {
    val loc = Location(10, 10)
    val temps: Iterable[(Location, Temperature)] = IndexedSeq(
      (Location(-20, -10), -10.5),
      (Location(0, 0), 45),
      (loc, 30))
    val colors: Iterable[(Temperature, Color)] = IndexedSeq(
      (50, Color(0, 0, 0)),
      (10, Color(255, 0, 255)))
    val expectedPixel = Pixel(80, 190, 128, 0, 128, 255)
    assertEquals(computePixel(80, 190, temps, colors), expectedPixel)
  }

  test("visualize with one temp and one color") {
    val temps: Iterable[(Location, Temperature)] = IndexedSeq(
      (Location(10, 10), 20))
    val colors: Iterable[(Temperature, Color)] = IndexedSeq((20, Color(100, 100, 100)))

    val result = visualize(temps, colors)
    assert(result.forAll(p => p.red() == 100 && p.blue() == 100 && p.green() == 100 && p.alpha() == 255))
  }

  test("visualize multiple temps and one color") {
    val temps: Iterable[(Location, Temperature)] = IndexedSeq(
      (Location(20, 120), 20),
      (Location(-10, -170), 5),
      (Location(50, 50), 26))
    val colors: Iterable[(Temperature, Color)] = IndexedSeq((5, Color(94, 123, 240)))

    val result = visualize(temps, colors)
    assert(result.forAll(p => p.red() == 94 && p.green() == 123 && p.blue() == 240 && p.alpha() == 255))
  }

  test("visualize one temp and multiple colors") {
    val temps: Iterable[(Location, Temperature)] = IndexedSeq((Location(20, 100), 3))
    val colors: Iterable[(Temperature, Color)] = IndexedSeq(
      (-1, Color(50, 50, 50)),
      (-0.5, Color(75, 75, 75)),
      (1, Color(99, 99, 99)),
      (5, Color(101, 101, 101)),
      (15, Color(150, 150, 150)))

    val result = visualize(temps, colors)
    assert(result.forAll(p => p.red() == 100 && p.blue() == 100 && p.green() == 100 && p.alpha() == 255))
  }

  test("visualize multiple temps and multiple colors") {
    val temps: Iterable[(Location, Temperature)] = IndexedSeq(
      (Location(90.0, -180.0), 30),
      (Location(-89.0, 179.0), -30))
    val colors: Iterable[(Temperature, Color)] = IndexedSeq(
      (-30, Color(255, 0, 0)),
      (30, Color(0, 0, 255)))

    val result = visualize(temps, colors)
    result.forEach(p => {
      val lat = 90 - p.x
      val longitude = p.y - 180
      if (lat <= longitude) {
        p.red() >= 128 && p.blue() <= 128
      } else {
        p.red() <= 128 && p.blue() >= 128
      }
    })
  }



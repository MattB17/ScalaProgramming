package observatory

import Visualization.*

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



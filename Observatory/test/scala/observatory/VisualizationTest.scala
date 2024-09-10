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



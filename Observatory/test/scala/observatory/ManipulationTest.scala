package observatory

import Manipulation.*
import Visualization.*

import scala.collection

trait ManipulationTest extends MilestoneSuite:
  private val milestoneTest = namedMilestoneTest("data manipulation", 4) _

  private val gridLocs =
    for
      lat <- -89 to 90
      lon <- -180 to 179
    yield GridLocation(lat, lon)

  test("makeGrid with one temperature") {
    val temps: Iterable[(Location, Temperature)] = IndexedSeq((Location(0, 0), 10))
    val gridFunc = makeGrid(temps)

    gridLocs.foreach(gl => assertEqualsDouble(gridFunc(gl), 10.0, 0.01))
  }

  test("makeGrid with multiple temperatures") {
    val loc0 = Location(90, -180)
    val loc1 = Location(-89, 179)
    val temps: Iterable[(Location, Temperature)] = IndexedSeq(
      (loc0, 20),
      (loc1, -20))
    val gridFunc = makeGrid(temps)

    gridLocs.foreach(gl => {
      val loc = Location(gl.lat, gl.lon)
      if (greatCircleDistanceAngle(loc, loc0) < greatCircleDistanceAngle(loc, loc1)) {
        assert(gridFunc(gl) >= 0)
      } else {
        assert(gridFunc(gl) <= 0)
      }
    })
  }

  test("average with one temperature list") {
    val temps: Iterable[Iterable[(Location, Temperature)]] = IndexedSeq(
      IndexedSeq((Location(0, 0), 20)))
    val gridAvgFunc = average(temps)

    gridLocs.foreach(gl => assertEqualsDouble(gridAvgFunc(gl), 20.0, 0.01))
  }

  test("average with multiple temperature lists") {
    val temps: Iterable[Iterable[(Location, Temperature)]] = IndexedSeq(
      IndexedSeq((Location(0, 0), 20)),
      IndexedSeq((Location(70, -160), 0)),
      IndexedSeq((Location(-15, 10), 10)))
    val gridAvgFunc = average(temps)

    gridLocs.foreach(gl => assertEqualsDouble(gridAvgFunc(gl), 10.0, 0.01))
  }

  test("deviations with simple normal") {
    val temps: Iterable[(Location, Temperature)] = IndexedSeq((Location(0, 0), 10))
    val norms: GridLocation => Temperature = gl => 9
    val devs = deviation(temps, norms)
    
    gridLocs.foreach(gl => assertEqualsDouble(devs(gl), 1.0, 0.01))
  }
  
  test("deviations with 2 element normal") {
    val temps: Iterable[(Location, Temperature)] = IndexedSeq((Location(30, 120), 20))
    val norms: GridLocation => Temperature = gl => {
      if (gl.lat >= 0) {
        10
      } else {
        30
      }
    }
    val devs = deviation(temps, norms)
    
    gridLocs.foreach(gl => {
      if (gl.lat >= 0) {
        assertEqualsDouble(devs(gl), 10.0, 0.01)
      } else {
        assertEqualsDouble(devs(gl), -10.0, 0.01)
      }
    })
  }
  
  test("deviations with complex normals") {
    val temps: Iterable[(Location, Temperature)] = IndexedSeq((Location(70, -120), 0))
    val norms: GridLocation => Temperature = gl => {
      if (gl.lat >= 0 && gl.lon < 0) {
        10
      } else if (gl.lat >= 0) {
        5
      } else if (gl.lon >= 0) {
        -5
      } else {
        -10
      }
    }
    val devs = deviation(temps, norms)
    
    gridLocs.foreach(gl => {
      if (gl.lat >= 0 && gl.lon < 0) {
        assertEqualsDouble(devs(gl), -10.0, 0.01)
      } else if (gl.lat >= 0) {
        assertEqualsDouble(devs(gl), -5.0, 0.01)
      } else if (gl.lon >= 0) {
        assertEqualsDouble(devs(gl), 5.0, 0.01)
      } else {
        assertEqualsDouble(devs(gl), 10.0, 0.01)
      }
    })
  }


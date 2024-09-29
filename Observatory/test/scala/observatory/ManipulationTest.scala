package observatory

import Manipulation.*
import Visualization.*

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


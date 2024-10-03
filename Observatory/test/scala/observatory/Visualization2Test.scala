package observatory
import Visualization2.*

trait Visualization2Test extends MilestoneSuite:
  private val milestoneTest = namedMilestoneTest("value-added information visualization", 5) _

  test("interpolateTemp x has value 0") {
    assertEqualsDouble(interpolateTemp(0.0, 12.0, 20.0), 12.0, 0.01)
  }

  test("interpolateTemp x has value 1") {
    assertEqualsDouble(interpolateTemp(1.0, 12.0, 20.0), 20.0, 0.01)
  }

  test("interpolateTemp closer to 0") {
    assertEqualsDouble(interpolateTemp(0.3, 10.0, 20.0), 13.0, 0.01)
  }

  test("interpolateTemp closer to 1") {
    assertEqualsDouble(interpolateTemp(0.7, 10.0, 20.0), 17.0, 0.01)
  }

  test("bilinearInterpolation all temps the same") {
    val p = CellPoint(0.3, 0.7)
    assertEqualsDouble(bilinearInterpolation(p, 10, 10, 10, 10), 10.0, 0.01)
  }

  test("bilinearInterpolation same temps on top and bottom") {
    val p = CellPoint(0.6, 0.5)
    // d00 and d10 both have value 0 so the linear interpolation of these 2 will be 7
    // d10 and d11 both have value 5 so the linear interpolation will have value 5
    // Then interpolating between these two values will give 7 * y + 5 * (1 - y) = 7 * 0.5 + 5 * 0.5 = 6
    assertEqualsDouble(bilinearInterpolation(p, 7, 5, 7, 5), 6.0, 0.01)
  }

  test("bilinear interpolation same temps on left and right") {
    val p = CellPoint(0.6, 0.3)
    // for d00 and d10 the interpolation is 10 * 0.4 + 20 * 0.6 = 16
    // the same holds for d01 and d11
    // so the bilinear interpolation is also 16
    assertEqualsDouble(bilinearInterpolation(p, 10, 10, 20, 20), 16.0, 0.01)
  }

  test("bilinear interpolation with 4 different temps") {
    val p = CellPoint(0.4, 0.7)
    // for d00 and d10 the interpolation is 5 * 0.6 + 10 * 0.4 = 7
    // for d01 and d11 the interpolation is 12 * 0.6 + 15 * 0.4 = 13.2
    // then the bilinear interpolation is 7 * 0.3 + 13.2 * 0.7 = 11.34
    assertEqualsDouble(bilinearInterpolation(p, 5, 12, 10, 15), 11.34, 0.001)
  }

  test("getTempForLocation all temps the same") {
    val l: Location = Location(70.3, 120.7)
    val gridFunc: GridLocation => Temperature = gl => 10
    assertEqualsDouble(getTempForLocation(l, gridFunc), 10.0, 0.01)
  }

  test("getTempForLocation location values are integer") {
    val l: Location = Location(25, 50)
    val gridFunc: GridLocation => Temperature = gl => {
      if (gl.lat == 25 && gl.lon == 50) {
        5
      } else if (gl.lat == 25) {
        4
      } else if (gl.lon == 50) {
        3
      } else {
        2
      }
    }

    assertEqualsDouble(getTempForLocation(l, gridFunc), 5.0, 0.01)
  }

  test("getTempForLocation same temps on top and bottom") {
    val l: Location = Location(10.6, 110.5)
    val gridFunc: GridLocation => Temperature = gl => {
      if (gl.lat == 10 && gl.lon == 110) {
        7
      } else if (gl.lat == 10) {
        5
      } else if (gl.lon == 110) {
        7
      } else {
        5
      }
    }
    assertEqualsDouble(getTempForLocation(l, gridFunc), 6.0, 0.01)
  }

  test("getTempForLocation same temps on left and right") {
    val l: Location = Location(86.6, 34.3)
    val gridFunc: GridLocation => Temperature = gl => {
      if (gl.lat == 86) {
        10
      } else {
        20
      }
    }
    assertEqualsDouble(getTempForLocation(l, gridFunc), 16.0, 0.01)
  }

  test("getTempForLocation with 4 different temps") {
    val l: Location = Location(0.4, 25.7)
    val gridFunc: GridLocation => Temperature = gl => {
      if (gl.lat == 0 && gl.lon == 25) {
        5
      } else if (gl.lat == 0) {
        12
      } else if (gl.lon == 25) {
        10
      } else {
        15
      }
    }
    assertEqualsDouble(getTempForLocation(l, gridFunc), 11.34, 0.001)
  }



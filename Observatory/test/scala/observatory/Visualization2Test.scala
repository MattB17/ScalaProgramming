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



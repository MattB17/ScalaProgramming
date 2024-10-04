package observatory
import Signal.Var
import Interaction2.*
import observatory.LayerName.{Deviations, Temperatures}

trait Interaction2Test extends MilestoneSuite:
  private val milestoneTest = namedMilestoneTest("interactive user interface", 6) _

  test("availableLayers correct names") {
    val result = availableLayers
    assert(result.size == 2)
    assert(result.head.layerName == LayerName.Temperatures)
    assert(result(1).layerName == LayerName.Deviations)
  }

  test("availableLayers has correct range") {
    val result = availableLayers
    result.foreach(l => assert(l.bounds == (1975 to 2015)))
  }

  test("availableLayers has correct number of colors") {
    val result = availableLayers
    assert(result.head.colorScale.size == 8)
    assert(result(1).colorScale.size == 6)
  }

  test("yearBounds extracts correct range") {
    val range1 = 1975 to 2015
    val sigLayer1 = Signal(Layer(Temperatures, Seq(), range1))
    assertEquals(yearBounds(sigLayer1).currentValue, range1)

    val range2 = 10 until 30
    val sigLayer2 = Signal(Layer(Deviations, Seq(), range2))
    assertEquals(yearBounds(sigLayer2).currentValue, range2)
  }

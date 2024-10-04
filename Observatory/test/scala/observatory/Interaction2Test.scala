package observatory
import Signal.Var
import Interaction2.*
import observatory.LayerName.{Deviations, Temperatures}

trait Interaction2Test extends MilestoneSuite:
  private val milestoneTest = namedMilestoneTest("interactive user interface", 6) _

  private val testSigLayer0 = Signal(Layer(Temperatures, Seq(), 1975 to 2000))
  private val testSigLayer1 = Signal(Layer(Deviations, Seq(), 1980 to 2020))

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

  test("yearSelection year in range") {
    assertEquals(yearSelection(testSigLayer0, Signal(1982)).currentValue, 1982)
    assertEquals(yearSelection(testSigLayer0, Signal(1975)).currentValue, 1975)
    assertEquals(yearSelection(testSigLayer0, Signal(2000)).currentValue, 2000)
  }

  test("yearSelection year below range") {
    assertEquals(yearSelection(testSigLayer0, Signal(1970)).currentValue, 1975)
    assertEquals(yearSelection(testSigLayer0, Signal(1899)).currentValue, 1975)
  }

  test("yearSelection year above range") {
    assertEquals(yearSelection(testSigLayer0, Signal(2003)).currentValue, 2000)
    assertEquals(yearSelection(testSigLayer0, Signal(2054)).currentValue, 2000)
  }

  test("layerUrlPattern temperatures layer") {
    assertEquals(
      layerUrlPattern(testSigLayer0, Signal(1980)).currentValue,
      "generated/temperatures/1980/{z}/{x}/{y}.png")

    assertEquals(
      layerUrlPattern(testSigLayer0, Signal(1995)).currentValue,
      "generated/temperatures/1995/{z}/{x}/{y}.png")
  }

  test("layerUrlPattern deviations layer") {
    assertEquals(
      layerUrlPattern(testSigLayer1, Signal(1990)).currentValue,
      "generated/deviations/1990/{z}/{x}/{y}.png")

    assertEquals(
      layerUrlPattern(testSigLayer1, Signal(2011)).currentValue,
      "generated/deviations/2011/{z}/{x}/{y}.png")
  }

  test("caption temperatures layer") {
    assertEquals(
      caption(testSigLayer0, Signal(1980)).currentValue,
      "Temperatures (1980)")

    assertEquals(
      caption(testSigLayer0, Signal(1995)).currentValue,
      "Temperatures (1995)")
  }

  test("caption deviations layer") {
    assertEquals(
      caption(testSigLayer1, Signal(1990)).currentValue,
      "Deviations (1990)")

    assertEquals(
      caption(testSigLayer1, Signal(2011)).currentValue,
      "Deviations (2011)")
  }

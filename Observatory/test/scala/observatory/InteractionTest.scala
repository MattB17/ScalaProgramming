package observatory

import scala.collection.concurrent.TrieMap
import java.awt.Point
import org.scalacheck.Prop
import org.scalacheck.Prop.{forAll, propBoolean}
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.Colors
import scala.math.BigDecimal.RoundingMode

trait InteractionTest extends MilestoneSuite:
  private val milestoneTest = namedMilestoneTest("interactive visualization", 3) _
  import Interaction.*

  test("tileLocation tile with no zoom") {
    val t = Tile(0, 0, 0)
    val result = tileLocation(t)
    assertEqualsDouble(result.lon, -180.0, 0.1)
    assertEqualsDouble(result.lat, 85.0511, 0.001)
  }

  test("tileLocation with small zoom") {
    val t = Tile(1, 0, 1)
    val result = tileLocation(t)
    assertEqualsDouble(result.lat, 85.0511, 0.001)
    assertEqualsDouble(result.lon, 0.0, 0.1)
  }

  test("tileLocation with large zoom") {
    val t = Tile(64, 32, 8)
    val result = tileLocation(t)
    assertEqualsDouble(result.lat, 85.0511, 0.001)
    assertEqualsDouble(result.lon, -90.0, 0.1)
  }

  test("getColorForSubtile one temp and one color") {
    val subtile = Tile(0, 0, 0)
    val temps: Iterable[(Location, Temperature)] = IndexedSeq(
      (Location(10, 10), 5))
    val colors: Iterable[(Temperature, Color)] = IndexedSeq(
      (20, Color(95, 110, 200)))
    val result = getColorForSubtile(temps, colors, subtile)

    assertEquals(result.red, 95)
    assertEquals(result.green, 110)
    assertEquals(result.blue, 200)
  }

  test("getColorForSubtile multiple temps and one color") {
    val subtile = Tile(1, 0, 1)
    val temps: Iterable[(Location, Temperature)] = IndexedSeq(
      (Location(80, -170), 20),
      (Location(5, 2), 5),
      (Location(-69, 137), 15))
    val colors: Iterable[(Temperature, Color)] = IndexedSeq((-5, Color(97, 103, 84)))
    val result = getColorForSubtile(temps, colors, subtile)

    assertEquals(result.red, 97)
    assertEquals(result.green, 103)
    assertEquals(result.blue, 84)
  }

  test("getColorForSubtile one temp and multiple colors") {
    val subtile = Tile(3, 4, 3)
    val temps: Iterable[(Location, Temperature)] = IndexedSeq((Location(0, 0), 5))
    val colors: Iterable[(Temperature, Color)] = IndexedSeq(
      (0, Color(255, 255, 255)),
      (10, Color(0, 0, 0)))
    val result = getColorForSubtile(temps, colors, subtile)

    assertEquals(result.red, 128)
    assertEquals(result.green, 128)
    assertEquals(result.blue, 128)
  }

  test("getColorForSubtile multiple temps and multiple colors") {
    val subtile = Tile(64, 32, 8)
    val temps: Iterable[(Location, Temperature)] = IndexedSeq(
      (Location(90, -180), 25),
      (Location(75, -110), 20),
      (Location(85.0511, -90), 3),
      (Location(0, 0), -37),
      (Location(-20, 110), 40))
    val colors: Iterable[(Temperature, Color)] = IndexedSeq(
      (20, Color(110, 11, 95)),
      (10, Color(45, 97, 211)),
      (5, Color(255, 0, 0)),
      (1, Color(0, 0, 255)),
      (-15, Color(213, 114, 7)))
    val result = getColorForSubtile(temps, colors, subtile)

    assertEquals(result.red, 128)
    assertEquals(result.green, 0)
    assertEquals(result.blue, 128)
  }

  test("tile one temp and one color") {
    val t = Tile(0, 0, 0)
    val temps: Iterable[(Location, Temperature)] = IndexedSeq((Location(0, 0), 15))
    val colors: Iterable[(Temperature, Color)] = IndexedSeq((5, Color(25, 111, 234)))
    val result = tile(temps, colors, t)

    assertEquals(result.height, 256)
    assertEquals(result.width, 256)
    assert(result.forAll(p => p.red() == 25 && p.green() == 111 && p.blue() == 234 && p.alpha() == 127))
  }

  test("tile one temp and multiple colors") {
    val t = Tile(3, 4, 3)
    val temps: Iterable[(Location, Temperature)] = IndexedSeq((Location(30, -20), 3))
    val colors: Iterable[(Temperature, Color)] = IndexedSeq(
      (1, Color(255, 0, 0)),
      (5, Color(0, 0, 255)))
    val result = tile(temps, colors, t)

    assertEquals(result.height, 256)
    assertEquals(result.width, 256)
    assert(result.forAll(p => p.red() == 128 && p.green() == 0 && p.blue() == 128 && p.alpha() == 127))
  }

  test("tile multiple temps and one color") {
    val t = Tile(64, 32, 8)
    val temps: Iterable[(Location, Temperature)] = IndexedSeq(
      (Location(20, 20), 25),
      (Location(-15, 100), 19),
      (Location(65, -37), 5))
    val colors: Iterable[(Temperature, Color)] = IndexedSeq((10, Color(157, 212, 6)))
    val result = tile(temps, colors, t)

    assertEquals(result.height, 256)
    assertEquals(result.width, 256)
    assert(result.forAll(p => p.red() == 157 && p.green() == 212 && p.blue() == 6 && p.alpha() == 127))
  }

  test("tile multiple temps and multiple colors") {
    val t = Tile(1, 1, 1)
    val temps: Iterable[(Location, Temperature)] = IndexedSeq(
      (Location(90, -180), -25),
      (Location(89, -175), -21),
      (Location(87, -179), -24),
      (Location(-45, 90), 15))
    val colors: Iterable[(Temperature, Color)] = IndexedSeq(
      (-30, Color(255, 255, 255)),
      (20, Color(0, 0, 0)))
    val result = tile(temps, colors, t)

    assertEquals(result.height, 256)
    assertEquals(result.width, 256)
    assert(result.forAll(p => p.red() >= 128 && p.green() >= 128 && p.blue() >= 128 && p.alpha() == 127))
  }


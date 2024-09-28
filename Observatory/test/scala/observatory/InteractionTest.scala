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
    assertEqualsDouble(result.lat, 79.1713, 0.001)
    assertEqualsDouble(result.lon, -90.0, 0.1)
  }


package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import scala.collection.parallel.CollectionConverters.given
import Interaction.*
import Visualization.*

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface:

  /**
   * Note that formally we are doing linear interpolation, but this function will only be used in
   * bilinear interpolation where x0 is 0 and x1 is 1. Thus we don't need to divide by x1 - x0 = 1
   * and multiplying temp1 by (x - x0) is the same as multiplying by x
   * 
   * @param x the coordinate of the target point being computed
   * @param temp0 the temperature at the point used for interpolation at coordinate 0
   * @param temp1 the temperature at the point used for interpolation at coordinate 1
   * @return the linear interpolation of the temperature at point x using temp0 at point 0 and temp1 at point 1.
   */
  def interpolateTemp(x: Double, temp0: Temperature, temp1: Temperature): Temperature = {
    val int0 = temp0 * (1.0 - x)
    val int1 = temp1 * x
    int0 + int1
  }

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    val t0 = interpolateTemp(point.x, d00, d10)
    val t1 = interpolateTemp(point.x, d01, d11)
    interpolateTemp(point.y, t0, t1)
  }

  def getTempForLocation(loc: Location, grid: GridLocation => Temperature): Temperature = {
    val xLeft: Int = math.floor(loc.lon).toInt
    val xRight: Int = math.ceil(loc.lon).toInt
    val yLow: Int = math.floor(loc.lat).toInt
    val yHigh: Int = math.ceil(loc.lat).toInt
    val d00: Temperature = grid(GridLocation(yLow, xLeft))
    val d01: Temperature = grid(GridLocation(yHigh, xLeft))
    val d10: Temperature = grid(GridLocation(yLow, xRight))
    val d11: Temperature = grid(GridLocation(yHigh, xRight))
    bilinearInterpolation(CellPoint(loc.lon - xLeft, loc.lat - yLow), d00, d01, d10, d11)
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): ImmutableImage = {
    val (w, h, a) = (256, 256, 127)
    val pixelCoords =
      for
        x <- 0 until h
        y <- 0 until w
      yield (x, y)

    val pixelArray = pixelCoords
      .par
      .map((x, y) => (x, y, Tile(y + tile.y * w, x + tile.x * h, tile.zoom + 8)))
      .map((x, y, t) => (x, y, tileLocation(t)))
      .map((x, y, l) => (x, y, getTempForLocation(l, grid)))
      .map((x, y, t) => (x, y, interpolateColor(colors, t)))
      .map((x, y, c) => Pixel(x, y, c.red, c.green, c.blue, a))
      .toArray

    ImmutableImage.wrapPixels(w, h, pixelArray, ImageMetadata.empty)
  }


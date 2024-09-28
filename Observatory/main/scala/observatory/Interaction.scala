package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import scala.collection.parallel.CollectionConverters.given
import Visualization.*

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface:

  /**
    * We convert a Tile to a Location using the Mercator projection:
    * https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Mathematics
    *
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val adj = math.pow(2, tile.zoom).toInt
    val lon = (tile.x * 360) / adj - 180.0
    val a = math.Pi - ((tile.y / adj) * 2.0 * math.Pi)
    val lat = math.atan(math.sinh(a))
    Location(math.toDegrees(lat), lon)
  }

  def getColorForSubtile(temps: Iterable[(Location, Temperature)],
                         cols: Iterable[(Temperature, Color)],
                         subtile: Tile): Color = {
    val loc = tileLocation(subtile)
    val temp = predictTemperature(temps, loc)
    interpolateColor(cols, temp)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): ImmutableImage = {
    val (w, h, a) = (256, 256, 127)
    val pixelCoords = for
      x <- 0 until h
      y <- 0 until w
    yield (x, y)

    val pixelArray = pixelCoords
      .par
      .map((x, y) => (x, y, Tile(y + tile.y * w, x + tile.x * h, tile.zoom + 8)))
      .map((x, y, subtile) => (x, y, getColorForSubtile(temperatures, colors, subtile)))
      .map((x, y, color) => Pixel(x, y, color.red, color.green, color.blue, a))
      .toArray

    ImmutableImage.wrapPixels(w, h, pixelArray, ImageMetadata.empty)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit =
    ???


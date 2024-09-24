package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import com.sksamuel.scrimage.implicits.given

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.given
import scala.collection.parallel.immutable.ParIterable

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface:

  private val earthRadiusKm = 6371
  /**
   * The great circle distance calculation is used to calculate the
   * distance between 2 points on a sphere where in this case the Earth
   * represents the sphere. Information on the calculation can be found
   * here: https://en.wikipedia.org/wiki/Great-circle_distance
   * @param loc1 The first Location in the calculation
   * @param loc2 The second Location in the calculation
   * @return the great circle distance between loc1 and loc2 in radians
   */
  def greatCircleDistanceAngle(loc1: Location, loc2: Location): Double = {
    if (loc1.lat == loc2.lat && loc1.lon == loc2.lon) {
      0
    } else if (loc1.lat == -loc2.lat &&
      ((loc1.lon == loc2.lon + 180) || (loc1.lon == loc2.lon - 180))) {
      math.Pi
    } else {
      val deltaLong = math.toRadians(math.abs(loc1.lon - loc2.lon))
      val phi1 = math.toRadians(loc1.lat)
      val phi2 = math.toRadians(loc2.lat)
      val a = math.sin(phi1) * math.sin(phi2)
      val b = math.cos(phi1) * math.cos(phi2) * math.cos(deltaLong)

      math.acos(a + b)
    }
  }

  /**
    * Predicts the temperature of `location` using `temperatures`. The temperature is a weighted average of
    * the temperatures where the weight is the inverse of the distance. We apply one modification where if the
    * distance is less than 1 for any location we just approximate the temperature with the temperature at that
    * location.
    *
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val distances = temperatures.par
      .map((loc, temp) => (earthRadiusKm * greatCircleDistanceAngle(location, loc), temp))
    val closeDistances = distances.filter((dist, temp) => dist < 1)
    if (closeDistances.nonEmpty) {
      closeDistances.head._2
    } else {
      val weights = distances.map((dist, temp) => ((1 / dist), temp))
      val numerator = weights.aggregate[Double](0.0)((currNum, pair) => currNum + (pair._1 * pair._2), _ + _)
      val denominator = weights.aggregate[Double](0.0)((currNum, pair) => currNum + pair._1, _ + _)
      numerator / denominator
    }
  }

  /**
   * The linear interpolation between (x0, y0), (x1, y1) for x
   * @param x The point for which we want the interpolation
   * @param x0 The x value of the point to the left of x used for interpolation
   * @param y0 The y value of the point to the left of x used for interpolation
   * @param x1 The x value of the point to the right of x used for interpolation
   * @param y1 The y value of the point to the right of x used for interpolation
   * @return
   */
  def getLinearInterpolation(x: Double, x0: Double, y0: Int, x1: Double, y1: Int): Int = {
    val int0 = y0 * (x1 - x)
    val int1 = y1 * (x - x0)
    val dist = (x1 - x0)
    math.round(((int0 + int1) / dist).toFloat)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val greaterTemps = points
      .filter((temp, color) => temp >= value)
      .toList
      .sortWith((pair1, pair2) => pair1._1 < pair2._1)
    val lessTemps = points
      .filter((temp, color) => temp < value)
      .toList
      .sortWith((pair1, pair2) => pair1._1 > pair2._1)
    if (greaterTemps.isEmpty) {
      lessTemps.head._2
    } else if (lessTemps.isEmpty) {
      greaterTemps.head._2
    } else {
      val (temp0, color0) = lessTemps.head
      val (temp1, color1) = greaterTemps.head
      val redResult = getLinearInterpolation(value, temp0, color0.red, temp1, color1.red)
      val greenResult = getLinearInterpolation(value, temp0, color0.green, temp1, color1.green)
      val blueResult = getLinearInterpolation(value, temp0, color0.blue, temp1, color1.blue)
      Color(redResult, greenResult, blueResult)
    }
  }

  /**
   * Computes the pixel at (x, y) based on the weather station `temperatures` and `colors`.
   * @param x The x coordinate of the pixel
   * @param y The y coordinate of the pixe
   * @param temperatures An Iterable of Location and Temperature pairs specifying the temperatures
   *                     at the locations of the weather stations
   * @param colors An Iterable of Temperature and Color pairs specifying the color scale for coloring temperatures
   * @return a pixel for (x, y) based on `temperatures` and `colors`
   */
  def computePixel(x: Int,
                   y: Int,
                   temperatures: Iterable[(Location, Temperature)],
                   colors: Iterable[(Temperature, Color)]): Pixel = {
    val loc = Location(90.0 - x, y - 180.0)
    val temp = predictTemperature(temperatures, loc)
    val pixelColor = interpolateColor(colors, temp)
    Pixel(x, y, pixelColor.red, pixelColor.green, pixelColor.blue, 255)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)],
                colors: Iterable[(Temperature, Color)]): ImmutableImage =
    ???




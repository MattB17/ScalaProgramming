package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import com.sksamuel.scrimage.implicits.given
import scala.collection.parallel.CollectionConverters.given

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface:

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
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature =
    ???

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color =
    ???

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): ImmutableImage =
    ???



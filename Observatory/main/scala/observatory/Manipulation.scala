package observatory

import scala.collection.parallel.CollectionConverters.given
import Visualization.*

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface:

  /**
    * Note we could use memoization. However, when rendering the images we will need to calculate
    * a temperature for every grid location. So instead we elect to calculate them all upfront and store
    * the result in a `val` with a getter.
    * 
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    val gridTempMap = (
      for
        lat <- -89 to 90
        lon <- -180 to 179
      yield ((lat, lon), predictTemperature(temperatures, Location(lat, lon))))
      .toMap
    gl => gridTempMap((gl.lat, gl.lon))
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature =
    ???

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature =
    ???




package observatory

import observatory.LayerName.{Deviations, Temperatures}

/**
  * 6th (and last) milestone: user interface polishing
  */
object Interaction2 extends Interaction2Interface:

  /**
    * @return The available layers of the application
    */
  def availableLayers: Seq[Layer] = {
    val yearRange: Range = 1975 to 2015
    val tempColors: Seq[(Temperature, Color)] = Seq(
      (60, Color(255, 255, 255)),
      (32, Color(255, 0, 0)),
      (12, Color(255, 255, 0)),
      (0, Color(0, 255, 255)),
      (-15, Color(0, 0, 255)),
      (-27, Color(255, 0, 255)),
      (-50, Color(33, 0, 107)),
      (-60, Color(0, 0, 0)))
    val tempLayer = Layer(Temperatures, tempColors, yearRange)

    val devColors: Seq[(Temperature, Color)] = Seq(
      (7, Color(0, 0, 0)),
      (4, Color(255, 0, 0)),
      (2, Color(255, 255, 0)),
      (0, Color(255, 255, 255)),
      (-2, Color(0, 255, 255)),
      (-7, Color(0, 0, 255)))
    val devLayer = Layer(Deviations, devColors, yearRange)

    Seq(tempLayer, devLayer)
  }

  /**
    * @param selectedLayer A signal carrying the layer selected by the user
    * @return A signal containing the year bounds corresponding to the selected layer
    */
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range] = {
    Signal(selectedLayer().bounds)
  }

  /**
    * @param selectedLayer The selected layer
    * @param sliderValue The value of the year slider
    * @return The value of the selected year, so that it never goes out of the layer bounds.
    *         If the value of `sliderValue` is out of the `selectedLayer` bounds,
    *         this method should return the closest value that is included
    *         in the `selectedLayer` bounds.
    */
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Year]): Signal[Year] = {
    Signal(
      sliderValue()
        .min(yearBounds(selectedLayer)().max)
        .max(yearBounds(selectedLayer)().min)
    )
  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The URL pattern to retrieve tiles
    */
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] =
    ???

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The caption to show
    */
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] =
    ???


// Interface used by the grading infrastructure. Do not change signatures
// or your submission will fail with a NoSuchMethodError.
trait Interaction2Interface:
  def availableLayers: Seq[Layer]
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range]
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Year]): Signal[Year]
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String]
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String]

enum LayerName:
  case Temperatures, Deviations
  def id: String =
    this match
      case Temperatures => "temperatures"
      case Deviations => "deviations"

/**
  * @param layerName Name of the layer
  * @param colorScale Color scale used by the layer
  * @param bounds Minimum and maximum year supported by the layer
  */
case class Layer(layerName: LayerName, colorScale: Seq[(Temperature, Color)], bounds: Range)


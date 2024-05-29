package calculator

object Polynomial extends PolynomialInterface:

  import java.lang.Math.sqrt

  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal({
      val aVal = a()
      val bVal = b()
      val cVal = c()
      (bVal * bVal) - (4 * aVal * cVal)
    })
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal({
      val aVal = a()
      val bVal = b()
      val d = delta()
      if (d < 0) {
        Set()
      } else if (d == 0) {
        Set(-bVal / (2 * aVal))
      } else {
        Set((-bVal + sqrt(d)) / (2 * aVal), (-bVal - sqrt(d)) / (2 * aVal))
      }
    })

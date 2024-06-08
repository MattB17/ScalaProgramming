package scalashop

import java.util.concurrent.*
import scala.collection.*

class BlurSuite extends munit.FunSuite:
  test("boxBlurKernel should correctly handle radius 0") {
    val src = new Img(7, 7)

    for (x <- 0 until 7; y <- 0 until 7)
      src(x, y) = rgba(x + 5, y * 2, x * y, (x * x) + (y * y))

    for (x <- 0 until 7; y <- 0 until 7)
      assert(boxBlurKernel(src, x, y, 0) == src(x, y))
  }

  test("boxBlurKernel with radius 1") {
    val src = new Img(3, 3)
    src(0, 0) = 0
    src(0, 1) = 1
    src(0, 2) = 2
    src(1, 0) = 4
    src(1, 1) = 5
    src(1, 2) = 8
    src(2, 0) = 10
    src(2, 1) = 10
    src(2, 2) = 50


    assert(boxBlurKernel(src, 1, 1, 1) == 10)
    assert(boxBlurKernel(src, 0, 1, 1) == 2)
    assert(boxBlurKernel(src, 1, 0, 1) == 4)
  }

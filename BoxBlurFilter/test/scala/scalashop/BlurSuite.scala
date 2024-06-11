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


    // The sum of each pixel divided by 9
    assert(boxBlurKernel(src, 1, 1, 1) == 10)

    // We'll sum the left column twice (because (-1, y) will be (0, y))
    // so its (0,0) + (0,1) + (0, 2) + (1,0) + (1,1) + (1,2)
    // all divided by 6
    assert(boxBlurKernel(src, 0, 1, 1) == 3)

    // We'll sum the top row twice (because (x, -1) will be clamped to (x, 0))
    // so its (0,0) + (1,0) + (2,0) + (0,1) + (1,1) + (2,1)
    // all divided by 6
    assert(boxBlurKernel(src, 1, 0, 1) == 5)

    // (3,2), (3,3), (2,3) and (2,2) will all be clamped to (2, 2),
    // (2, 1) and (3, 1) will be clamped to (2, 1),
    // (1, 2) and (1, 3) will be clamped to (1, 2)
    // so it's (2,2) + (2,1) + (1,2) + (1,1) all divided by 4
    assert(boxBlurKernel(src, 2, 2, 1) == 18)
  }

  test("boxBlurKernel should return correct value of interior edge pixel") {
    val src = new Img(3, 4)
    src(0, 0) = 0
    src(1, 0) = 1
    src(2, 0) = 2
    src(0, 1) = 3
    src(1, 1) = 4
    src(2, 1) = 5
    src(0, 2) = 6
    src(1, 2) = 7
    src(2, 2) = 8
    src(0, 3) = 50
    src(1, 3) = 11
    src(2, 3) = 16

    assert(boxBlurKernel(src, 0, 2, 1) == 13)
  }

  test("VerticalBoxBlur.blur with radius 0 should copy the strip") {
    val src = new Img(7, 7)
    val dst = new Img(7, 7)

    for (x <- 0 until 7; y <- 0 until 7)
      src(x, y) = rgba(x + 5, y * 2, x * y, (x * x) + (y * y))

    VerticalBoxBlur.blur(src, dst, 0, 2, 0)

    for (x <- 0 until 2; y <- 0 until 7)
      assert(dst(x, y) == src(x, y))

    for (x <- 2 until 7; y <- 0 until 7)
      assert(dst(x, y) == 0)
  }

  test("VerticalBoxBlur.parBlur with radius 0 should copy the whole image") {
    val src = new Img(7, 7)
    val dst = new Img(7, 7)

    for (x <- 0 until 7; y <- 0 until 7)
      src(x, y) = rgba(x + 5, y * 2, x * y, (x * x) + (y * y))

    VerticalBoxBlur.parBlur(src, dst, 4, 0)

    for (x <- 0 until 7; y <- 0 until 7)
      assert(dst(x, y) == src(x, y))
  }

  test("VerticalBoxBlur.blur with radius 1 image") {
    val src = new Img(3, 3)
    val dst = new Img(3, 3)
    src(0, 0) = 0
    src(0, 1) = 1
    src(0, 2) = 2
    src(1, 0) = 4
    src(1, 1) = 5
    src(1, 2) = 8
    src(2, 0) = 10
    src(2, 1) = 10
    src(2, 2) = 50

    VerticalBoxBlur.blur(src, dst, 0, 3, 1)

    assert(dst(0, 0) == 2)
    assert(dst(0, 1) == 3)
    assert(dst(0, 2) == 4)
    assert(dst(1, 0) == 5)
    assert(dst(1, 1) == 10)
    assert(dst(1, 2) == 12)
    assert(dst(2, 0) == 7)
    assert(dst(2, 1) == 14)
    assert(dst(2, 2) == 18)
  }

  test("VerticalBoxBlur.parBlur with radius 2 and 3 tasks") {
    val src = new Img(4, 3)
    val dst = new Img(4, 3)
    src(0, 0) = 0
    src(1, 0) = 1
    src(2, 0) = 2
    src(3, 0) = 9
    src(0, 1) = 3
    src(1, 1) = 4
    src(2, 1) = 5
    src(3, 1) = 10
    src(0, 2) = 6
    src(1, 2) = 7
    src(2, 2) = 8
    src(3, 2) = 11

    VerticalBoxBlur.parBlur(src, dst, 3, 2)

    assert(dst(0, 0) == 4)
    assert(dst(1, 0) == 5)
    assert(dst(2, 0) == 5)
    assert(dst(3, 0) == 6)
    assert(dst(0, 1) == 4)
    assert(dst(1, 1) == 5)
    assert(dst(2, 1) == 5)
    assert(dst(3, 1) == 6)
    assert(dst(0, 2) == 4)
    assert(dst(1, 2) == 5)
    assert(dst(2, 2) == 5)
    assert(dst(3, 2) == 6)
  }

  test("HorizontalBoxBlur.blur with radius 0 should copy the strip") {
    val src = new Img(7, 7)
    val dst = new Img(7, 7)

    for (x <- 0 until 7; y <- 0 until 7)
      src(x, y) = rgba(x + 5, y * 2, x * y, (x * x) + (y * y))

    HorizontalBoxBlur.blur(src, dst, 0, 2, 0)

    for (y <- 0 until 2; x <- 0 until 7)
      assert(dst(x, y) == src(x, y))

    for (y <- 2 until 7; x <- 0 until 7)
      assert(dst(x, y) == 0)
  }

  test("HorizontalBoxBlur.parBlur with radius 0 should copy the whole image") {
    val src = new Img(7, 7)
    val dst = new Img(7, 7)

    for (x <- 0 until 7; y <- 0 until 7)
      src(x, y) = rgba(x + 5, y * 2, x * y, (x * x) + (y * y))

    HorizontalBoxBlur.parBlur(src, dst, 4, 0)

    for (x <- 0 until 7; y <- 0 until 7)
      assert(dst(x, y) == src(x, y))
  }

  test("HorizontalBoxBlur.blur with radius 1 image") {
    val src = new Img(3, 3)
    val dst = new Img(3, 3)
    src(0, 0) = 0
    src(0, 1) = 1
    src(0, 2) = 2
    src(1, 0) = 4
    src(1, 1) = 5
    src(1, 2) = 8
    src(2, 0) = 10
    src(2, 1) = 10
    src(2, 2) = 50

    HorizontalBoxBlur.blur(src, dst, 0, 3, 1)

    assert(dst(0, 0) == 2)
    assert(dst(0, 1) == 3)
    assert(dst(0, 2) == 4)
    assert(dst(1, 0) == 5)
    assert(dst(1, 1) == 10)
    assert(dst(1, 2) == 12)
    assert(dst(2, 0) == 7)
    assert(dst(2, 1) == 14)
    assert(dst(2, 2) == 18)
  }

  test("HorizontalBoxBlur.parBlur with radius 2 and 3 tasks") {
    val src = new Img(4, 3)
    val dst = new Img(4, 3)
    src(0, 0) = 0
    src(1, 0) = 1
    src(2, 0) = 2
    src(3, 0) = 9
    src(0, 1) = 3
    src(1, 1) = 4
    src(2, 1) = 5
    src(3, 1) = 10
    src(0, 2) = 6
    src(1, 2) = 7
    src(2, 2) = 8
    src(3, 2) = 11

    HorizontalBoxBlur.parBlur(src, dst, 3, 2)

    assert(dst(0, 0) == 4)
    assert(dst(1, 0) == 5)
    assert(dst(2, 0) == 5)
    assert(dst(3, 0) == 6)
    assert(dst(0, 1) == 4)
    assert(dst(1, 1) == 5)
    assert(dst(2, 1) == 5)
    assert(dst(3, 1) == 6)
    assert(dst(0, 2) == 4)
    assert(dst(1, 2) == 5)
    assert(dst(2, 2) == 5)
    assert(dst(3, 2) == 6)
  }

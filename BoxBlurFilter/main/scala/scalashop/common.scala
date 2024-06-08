package scalashop

import java.util.concurrent.*
import scala.util.DynamicVariable

import org.scalameter.*

/** The value of every pixel is represented as a 32 bit integer. */
/** So an RGBA value is an integer of the form <r><g><b><a> where
 * <r> is the most significant 8 bits representing the red component
 * <g> is the next 8 bits representing the green component
 * <b> is the following 8 bits for the blue component
 * <a> is the last 8 bits for the transparent component */
type RGBA = Int

/** Returns the red component. */
/** This extracts the most significant 8 bits */
def red(c: RGBA): Int = (0xff000000 & c) >>> 24

/** Returns the green component. */
/** Truncates the least significant 16 bits and gets the next 8 bits */
def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

/** Returns the blue component. */
/** Truncates the least significant 8 bits and gets the next 8 bits */
def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

/** Returns the alpha component. */
/** Gets the least significant 8 bits */
def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

/** Used to create an RGBA value from separate components. */
def rgba(r: Int, g: Int, b: Int, a: Int): RGBA =
  // Puts r as the most significant 8 bits, g as the next 8 bits,
  // followed by 8 bits representing b, and lastly the least significant
  // 8 digits are a
  (r << 24) | (g << 16) | (b << 8) | (a << 0)

/** Restricts the integer into the specified range. */
def clamp(v: Int, min: Int, max: Int): Int =
  if v < min then min
  else if v > max then max
  else v

/** Image is a two-dimensional matrix of pixel values. */
class Img(val width: Int, val height: Int, private val data: Array[RGBA]):
  def this(w: Int, h: Int) = this(w, h, new Array(w * h))
  def apply(x: Int, y: Int): RGBA = data(y * width + x)
  def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c

/** Computes the blurred RGBA value of a single pixel of the input image. */
def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
  val srcWidth = src.width
  val srcHeight = src.height

  var redSum, greenSum, blueSum, alphaSum, pxCount = 0L
  var currX = x - radius
  var currY = y - radius

  while (currX <= x + radius) {
    while (currY <= y + radius) {
      val currPx = src(clamp(currX, 0, srcWidth), clamp(currY, 0, srcHeight))
      redSum += red(currPx)
      greenSum += green(currPx)
      blueSum += blue(currPx)
      alphaSum += alpha(currPx)
      pxCount += 1
      currY += 1
    }
    currX += 1
    currY = y - radius
  }
  rgba(
    (redSum / pxCount).toInt,
    (greenSum / pxCount).toInt,
    (blueSum / pxCount).toInt,
    (alphaSum / pxCount).toInt)
}

val forkJoinPool = ForkJoinPool()

abstract class TaskScheduler:
  def schedule[T](body: => T): ForkJoinTask[T]
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())

class DefaultTaskScheduler extends TaskScheduler:
  def schedule[T](body: => T): ForkJoinTask[T] =
    val t = new RecursiveTask[T] {
      def compute = body
    }
    Thread.currentThread match
      case wt: ForkJoinWorkerThread =>
        t.fork()
      case _ =>
        forkJoinPool.execute(t)
    t

val scheduler =
  DynamicVariable[TaskScheduler](DefaultTaskScheduler())

def task[T](body: => T): ForkJoinTask[T] =
  scheduler.value.schedule(body)

def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
  scheduler.value.parallel(taskA, taskB)

def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) =
  val ta = task { taskA }
  val tb = task { taskB }
  val tc = task { taskC }
  val td = taskD
  (ta.join(), tb.join(), tc.join(), td)

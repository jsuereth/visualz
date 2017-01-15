package com.jsuereth.visualz.plasma

/**
  * A noise/normalization technique that can be used to create terrain-maps or plasma-looking effects.
  */
class DiamondSquareInterpolator(rand: java.util.Random = new java.util.Random()) {
  import DiamondSquareInterpolator._

  /** Returns a float between [-1,1] */
  private def nextFloat: Float = (rand.nextFloat - 0.5f) * 2



  def diamondStep(data: Array[Array[Float]], startx: Int, starty: Int, size: Int, randMag: Float): Unit = {
    val midpoint = size / 2
    val average =
      (data(startx)(starty) +
      data(startx + size - 1)(starty) +
      data(startx + size - 1)(starty + size - 1) +
      data(startx)(starty + size - 1)) / 4.0f
    data(startx+midpoint)(starty+midpoint) = average + (randMag * nextFloat)
  }


  /** Calculuates a point at (x,y) coordinate, using the average from the diamond around the point.
    *
    * @param data  The data to mutate.
    * @param x
    * @param y
    * @param size  The "size" of the dimaond (i.e. dhow far to reach for points).
    * @param randMag  The magnitude of randomness to add this step.
    */
  def squareStep(data: Array[Array[Float]], x: Int, y: Int, size: Int, randMag: Float): Unit = {
    val midpoint = size / 2
    val fullSize = data.length
    // We don't wrap, because we don't ensure wrapped points have computed values yet.
    val leftX = x-midpoint //((x - midpoint) + fullSize) % fullSize
    val rightX = x + midpoint //((x + midpoint) + fullSize) % fullSize
    val leftY = y - midpoint //((y - midpoint) + fullSize) % fullSize
    val rightY = y + midpoint //((y + midpoint) + fullSize) % fullSize
    val values =
      Array((leftX, y), (rightX, y), (x, rightY), (x, leftY))
        .filterNot({ case (x,y) => (x >= fullSize || x < 0) || (y >= fullSize || y < 0)})
          .map({ case (x,y) =>
            data(x)(y)
          })
    val average = values.sum / values.length
    data(x)(y) = average + (randMag * nextFloat)
  }

  def interpolate(data: Array[Array[Float]]): Unit = {
    //  We need to run the current step COMPLETELY before we go further down, because otherwise we get
    // square artifacts as some steps look across quadrants.
    @annotation.tailrec
    def run(steps: List[InterpolateStep]): Unit = steps match {
      case (step @ InterpolateStep(size, startx, starty, randMag)) :: remaining =>
        diamondStep(data, startx, starty, size, randMag)
        // Four square steps, next
        val x1 = startx + (size/2)
        val y1 = starty + (size/2)
        val x2 = startx + size - 1
        val y2 = starty + size -1
        squareStep(data, startx, y1, size, randMag)
        squareStep(data, x1, starty, size, randMag)
        squareStep(data, x1, y2, size, randMag)
        squareStep(data, x2, y1, size, randMag)
        if (size > 3) {
          // Continue down...
          val newSize = (size / 2) + 1
          val newMag = randMag * 0.5f
          run(remaining ++
            List[InterpolateStep](
              InterpolateStep(newSize, startx, starty, newMag),
              InterpolateStep(newSize, startx + newSize - 1, starty, newMag),
              InterpolateStep(newSize, startx, starty + newSize - 1, newMag),
              InterpolateStep(newSize, startx + newSize - 1, starty + newSize - 1, newMag)))
        } else run(remaining)
      case Nil => ()
    }


    val size = data.length
    assert(isPowerOfTwo(size - 1))
    run(InterpolateStep(size, 0, 0, 1f) :: Nil)
  }
}


case class InterpolateStep(size: Int, startx: Int, starty: Int, randMag: Float) {
  override def toString = s"interpolation step @ ($startx, $starty) - $size x $size by $randMag"
}

object DiamondSquareInterpolator {

  private val powersOfTwo = Array(1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,
  65536,131072,262144,524288,1048576,2097152,4194304,8388608,
  16777216,33554432,67108864,134217728,268435456,536870912,
  1073741824)

  def isPowerOfTwo(size: Int): Boolean = powersOfTwo contains size

  def default = new DiamondSquareInterpolator()
}
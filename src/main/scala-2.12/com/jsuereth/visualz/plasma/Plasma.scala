package com.jsuereth.visualz.plasma

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

/**
  * Creates a plasma effect.
  */
object Plasma {
  def main(args: Array[String]): Unit = {
    val width = 256
    val height = 256
    val perlinWidth = 50
    val perlinHeight = 50
    val img = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
    val perlin = PerlinNoise.random(perlinWidth, perlinHeight)
    def color(noise: Float): Int = {
      val size = (noise * 100).toInt + 100
      (new java.awt.Color(size, size, size)).getRGB
    }
    for {
      x <- 0 until width
      y <- 0 until height
      px = (x.toFloat / perlinWidth)
      py = (y.toFloat / perlinHeight)
      noise = perlin.noise(px,py)
    } img.setRGB(x,y,color(noise))


    val outputfile = new File("plasma.png")
    ImageIO.write(img, "png", outputfile)
  }
}


/**
  * A class to compute perline noise.
 *
  * @param width  The width of the gradient vector (second array)
  * @param height The height of hte gradient vector (first array)
  * @param gradient A vector of gradients - matrix(y)(x)(0 or 1)
  */
// Precomputed (or otherwise) gradient vectors at each grid node
class PerlinNoise(width: Int, height: Int, gradient: (Int, Int) => (Float, Float)) {
  /** Function to linearly interpolate between a0 and a1
   * Weight w should be in the range [0.0, 1.0]
   */
  def lerp(a0: Float, a1: Float, w: Float): Float = (1.0f - w)*a0 + w*a1


  /** Computes the dot product of the distance and gradient vectors. */
  def dotGridGradient(ix: Int, iy: Int, x: Float, y: Float): Float = {
    // Compute the distance vector
    val dx = x - ix.toFloat
    val dy = y - iy.toFloat

    // Compute the dot-product
    val (gx, gy) = gradient(ix, iy)
    (dx*gx + dy*gy)
  }

  // Compute Perlin noise at coordinates (x, y)
  def noise(x: Float, y: Float): Float = {

    // Determine grid cell coordinates
    val x0 = if (x > 0.0) x.toInt else x.toInt - 1
    val x1 = x0 + 1
    val y0 = if (y > 0.0) y.toInt else y.toInt - 1
    val y1 = y0 + 1

    // Determine interpolation weights
    // Could also use higher order polynomial/s-curve here
    val sx = x - x0.toFloat
    val sy = y - y0.toFloat

    // Interpolate between grid point gradients
    val ix0 = {
      val n0 = dotGridGradient(x0, y0, x, y)
      val n1 = dotGridGradient(x1, y0, x, y)
      lerp(n0, n1, sx)
    }
    val ix1 = {
      val n0 = dotGridGradient(x0, y1, x, y)
      val n1 = dotGridGradient(x1, y1, x, y)
      lerp(n0, n1, sx)
    }
    lerp(ix0, ix1, sy)
  }
}
object PerlinNoise {
  def random(width: Int, height: Int): PerlinNoise = {
    val rand = new java.util.Random
    def randFloat = rand.nextFloat()
    val gradients =
      (for {
        x <- 0 until width
        y <- 0 until height
      } yield (randFloat, randFloat))

    new PerlinNoise(width, height, {
      (x,y) =>
        val rx = (x + width) % width
        val ry = (y + height) % height
        gradients(rx*width + ry)
    })
  }
}

package com.jsuereth.visualz.plasma

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

/**
  * Creates a plasma effect.
  */
object Plasma {
  /** Normalizes an array on a [0,1) space. */
  def normalizeArray(data: Array[Array[Float]]): Unit = {
    val min = data.iterator.map(_.min).min
    val max = data.iterator.map(_.max).max
    def normPoint(x: Float): Float = {
      (x - min) / (max - min)
    }
    for {
      r <- 0 until data.length
      c <- 0 until data(r).length
    } data(r)(c) = normPoint(data(r)(c))

  }

  def main(args: Array[String]): Unit = {
    val width = 128
    val height = 128
    val time = 32
    val noiseGen = SimplexNoise.default
    def color(noise: Double): Int = {
      val size = (noise * 255).toInt
      ((new java.awt.Color(size, size, size)).getRGB)
    }
    def wrap(t: Int): Int = {
      if (t > (time/2)) time - t else t
    }/*
    for (t <- 0 until time) {
      val img = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
      for {
        x <- 0 until width
        y <- 0 until height
        px = (x.toFloat / width)
        py = (y.toFloat / height)
        pt = 0.5f + (wrap(t) * 0.1f)
        noise = noiseGen(px, py, pt)
      } img.setRGB(x, y, color(noise))
      val outputfile = new File(s"plasma-${t}.png")
      ImageIO.write(img, "png", outputfile)
    }
    */
    val data =
      (for (i <- 0 to width) yield {
        (for (j <- 0 to height) yield  0.0f).toArray[Float]
      }).toArray[Array[Float]]
    data(0)(0) = (math.random.toFloat - 0.5f)*2.0f
    data(width-1)(height-1) = (math.random.toFloat - 0.5f)*2.0f
    data(0)(height-1) = (math.random.toFloat - 0.5f)*2.0f
    data(width-1)(0) = (math.random.toFloat - 0.5f)*2.0f
    DiamondSquareInterpolator.default.interpolate(data)
    normalizeArray(data)
    locally {
      val img = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
      for {
        x <- 0 until width
        y <- 0 until height
      } img.setRGB(x, y, color(data(x)(y)))
      val outputfile = new File(s"plasma-field.png")
      ImageIO.write(img, "png", outputfile)
    }
  }
}



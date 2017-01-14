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
    val time = 16
    val noiseGen = SimplexNoise.default
    def color(noise: Double): Int = {
      val size = (noise * 100).toInt + 100
      (new java.awt.Color(size, size, size)).getRGB
    }
    for (t <- 0 until time) {
      val img = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
      for {
        x <- 0 until width
        y <- 0 until height
        px = (x.toFloat / width)
        py = (y.toFloat / height)
        pt = 0.5f + (t * 0.1f)
        noise = noiseGen(px, py, pt)
      } img.setRGB(x, y, color(noise))
      val outputfile = new File(s"plasma-${t}.png")
      ImageIO.write(img, "png", outputfile)
    }
  }
}



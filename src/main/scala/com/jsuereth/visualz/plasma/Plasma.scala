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
    val noiseGen = SimplexNoise.random()
    val img = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
    def color(noise: Double): Int = {
      val size = (noise * 100).toInt + 100
      (new java.awt.Color(size, size, size)).getRGB
    }
    for {
      x <- 0 until width
      y <- 0 until height
      px = (x.toFloat / width)
      py = (y.toFloat / height)
      noise = noiseGen(px,py)
    } img.setRGB(x,y,color(noise))


    val outputfile = new File("plasma.png")
    ImageIO.write(img, "png", outputfile)
  }
}



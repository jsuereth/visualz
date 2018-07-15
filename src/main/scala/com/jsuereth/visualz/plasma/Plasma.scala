package com.jsuereth.visualz.plasma

import javax.imageio.ImageIO
import java.io.File
import java.awt.Color
import java.awt.image.BufferedImage
import java.lang.Math

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


  def whitescaleRGB(noise: Float): Int = {
    val amount = (noise*255).toInt
    new Color(255, 255, 255, amount).getRGB
  }

  def colorWithMinCutOff(cutOff: Float, color: Float => Int): Float => Int = noise => {
    if (noise < cutOff) new Color(0,0,0,0).getRGB
    else color(noise - cutOff)
  }

  /** Converts a frame of noise values to a frame of colors. */
  def colorize(frame: Frame[Float], color: Float => Int): Frame[Int] = {
    frame map color
  }


  /** Writes a frame of RGB values. */
  def writeImage(file: File, frame: Frame[Int]): Unit = {
    val img = new BufferedImage(frame.width, frame.height, BufferedImage.TYPE_4BYTE_ABGR)
    for {
      x <- 0 until frame.width
      y <- 0 until frame.height
    } img.setRGB(x,y, frame.value(x,y))
    ImageIO.write(img, "png", file)
  }

  def perlinNoise(width: Int, height: Int, ticks: Int): Seq[Frame[Float]] = {
    val noiseGen = SimplexNoise.default
    for (t <- 0 until ticks) yield {
      val frame = new Frame[Float](width, height)
      for {
        x <- 0 until width
        y <- 0 until height
        px = (x.toFloat / width)
        py = (y.toFloat / height)
        pt = 1.0f + (t * 0.05f)
        noise = noiseGen(px, py, pt)
      } frame.write(x, y, noise.toFloat)
      frame
    }
  }

  def xyRadius(x: Int, y: Int, width: Int, height: Int): Float = {
    val realX = (x - (width/2)) / width.toFloat
    val realY = (y - (height/2)) / height.toFloat
    Math.sqrt(realX*realX + realY*realY).toFloat
  }

  def ballMergeFrame(frame: Frame[Float], radius: Float, cutoff: Float): Unit = {
    for {
      x <- 0 until frame.width
      y <- 0 until frame.height
      normDistanceToEdge = (radius - xyRadius(x,y, frame.width, frame.height)) / radius.toFloat
      baseValue = if (normDistanceToEdge < cutoff) 0.0f else (normDistanceToEdge * frame.value(x,y) + 0.2)
    } frame.write(x,y, baseValue.toFloat)
  }

  def ballMerge(frames: Seq[Frame[Float]]): Seq[Frame[Float]] = {
    val maxLength = 0.5
    val ticks = frames.length
    for ((frame,idx) <- frames.iterator.zipWithIndex) {
      val halfWidth = frame.width / 2.0f
      val halfHeight = frame.height / 2.0f
      // TODO - adaptive circle.
      val maxLength = xyRadius(frame.width, frame.height, frame.width, frame.height)
      val growingFactor = Math.sin((idx / ticks.toFloat)* Math.PI)
      val radius = maxLength * 0.7f
      ballMergeFrame(frame, radius.toFloat, 0.15f)
    }
    frames
  }

  // Take all values which fall below the min and set them to 0.0f.  Then alter the range of all values appropriately.
  def minCutoff(frames: Seq[Frame[Float]], min: Float): Unit = {
    for (frame <- frames.iterator) {
      val spread = frame.maxValue - frame.minValue
      val factor = (spread) / (spread - min)
      def adjust(in: Float): Float =
        if (in < min) 0.0f
        else (in - min) * factor
      frame mutMap adjust
    }
  }

  def doublePerlin(width: Int, height: Int, time: Int): Seq[Frame[Float]] = {
    val one = perlinNoise(width, height, time)
    val two = perlinNoise(width, height, time)
    for ((l,r) <- one zip two) yield {
      val result = new Frame[Float](width, height)
      for {
        x <- 0 until l.width
        y <- 0 until l.height
      } result.write(x,y, l.value(x,y)*r.value(x,y))
      result
    }
  }

  def renderSpellBall(): Unit = {
    val width = 32
    val height = 32
    val time = 16
    val dir = new File("texture")
    if (!dir.exists) dir.mkdir()
    val frames = doublePerlin(width, height, time)
    frames.foreach(_.normalize)
    ballMerge(frames)
    frames foreach (_.normalize)
    val colored = frames map (f => colorize(f, whitescaleRGB))
    // render for GIF generation
    for ((frame, idx) <- colored.iterator.zipWithIndex) {
      val file = new File(dir, s"plasma-$idx.png")
      // Colorize and render
      writeImage(file, frame)
    }
    // render for texxture usage.
    val uberFrame = new Frame[Int](width, height*time)
    for {
      (frame,tick) <- colored.iterator.zipWithIndex
      x <- 0 until width
      y <- 0 until height
    } uberFrame.write(x, y + height*tick, frame.value(x,y))
    writeImage(new File(dir, s"ball.png"), uberFrame)
  }


  def main(args: Array[String]): Unit = {
    renderSpellBall()
/*
    val width = 128
    val height = 128
    val time = 32
    def color(noise: Double): Int = {
      // Specifically for diamond-square, let's do choose different colors based on height.
      if (noise < 0.4f) {
        // Ocean blue
        val simple = (noise * 255).toInt
        new Color(simple/2+60, simple/2+60, 100+simple).getRGB
      } else if (noise < 0.8f) {
        // Green
        val simple = ((noise - 0.4f) * 128).toInt
        new Color(simple/2+100, simple+100, simple/2+100).getRGB
      } else {
        // Brown?
        val simple = ((noise - 0.8f) * 256).toInt
        ((new java.awt.Color(simple*2+100, simple*2+100, simple+100)).getRGB)
      }
    }
    def wrap(t: Int): Int = {
      if (t > (time/2)) time - t else t
    }
    
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
*/
  }
}



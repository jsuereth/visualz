package com.jsuereth.visualz.plasma

import org.specs2._

class DiamondSquareTest extends Specification { def is = s2"""

 DiamondSquare interpolations should
   checks powers of two                 $checkPowersOfTwo
   calculate diamond average of 3x3     $checkDiamondStepSmall
   calculate square step of 3x3         $basicSquareStep
"""


  def isPowerOfTwo(size: Int) = DiamondSquareInterpolator.isPowerOfTwo(size) must beEqualTo(true)

  def checkPowersOfTwo =
    isPowerOfTwo(2) && isPowerOfTwo(32) && not(isPowerOfTwo(3)) && not(isPowerOfTwo(7))

  object fakeRandom extends java.util.Random {
    override def nextInt(x: Int): Int = 0
    override def nextFloat: Float = 0.0f
  }
  val ds = new DiamondSquareInterpolator(fakeRandom)

  def basicSquareStep = {
    val data =
      Array(
        Array(0f, 1f, 0f),
        Array(1f, 0f, 1f),
        Array(0f, 1f, 0f)
      )
    ds.squareStep(data, 1, 1, 3, 0.0f)

    data must beEqualTo(
      Array(
        Array(0f, 1f, 0f),
        Array(1f, 1f, 1f),
        Array(0f, 1f, 0f)
      )
    )
  }

  def checkDiamondStepSmall = {
    def step2 = {
      val data =
        Array(
          Array(0f, 0f, 1f),
          Array(0f, 0f, 0f),
          Array(1f, 0f, 0f)
        )
      ds.diamondStep(data, 0, 0, 3, 0.0f)

      data must beEqualTo(
        Array(
          Array(0f, 0f, 1f),
          Array(0f, .5f, 0f),
          Array(1f, 0f, 0f)
        )
      )
    }
    def step1 = {
      val data =
        Array(
          Array(1f, 0f, 1f),
          Array(0f, 0f, 0f),
          Array(1f, 0f, 1f)
        )
      ds.diamondStep(data, 0, 0, 3, 0.0f)

      data must beEqualTo(
        Array(
          Array(1f, 0f, 1f),
          Array(0f, 1f, 0f),
          Array(1f, 0f, 1f)
        )
      )
    }
    step1 and step2
  }
}

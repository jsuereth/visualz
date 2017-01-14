package com.jsuereth.visualz.plasma

// three dimensional gradient
case class Gradient(x: Int, y: Int, z: Int)

/*
 * A speed-improved simplex noise algorithm for 2D, 3D and 4D in Java.
 *
 * Based on example code by Stefan Gustavson (stegu@itn.liu.se).
 * Optimisations by Peter Eastman (peastman@drizzle.stanford.edu).
 * Better rank ordering method for 4D by Stefan Gustavson in 2012.
 *
 * This could be speeded up even further, but it's useful as it is.
 *
 * Version 2012-03-09
 *
 * This code was placed in the public domain by its original author,
 * Stefan Gustavson. You may use it as you see fit, but
 * attribution is appreciated.
 *
 *
 * Note: from http://staffwww.itn.liu.se/~stegu/simplexnoise/SimplexNoise.java
 */
object SimplexNoise {
  private val grad3 = Array(Gradient(1, 1, 0), Gradient(-1, 1, 0), Gradient(1, -1, 0), Gradient(-1, -1, 0),
    Gradient(1, 0, 1), Gradient(-1, 0, 1), Gradient(1, 0, -1), Gradient(-1, 0, -1),
    Gradient(0, 1, 1), Gradient(0, -1, 1), Gradient(0, 1, -1), Gradient(0, -1, -1))

  // To remove the need for index wrapping, double the permutation table length
  private val perm: Array[Short] = (0 until 512).map(i => (i.toShort & 255).toShort).toArray[Short]
  private val permMod12: Array[Short] = (0 until 512).map(i => (perm(i) % 12).toShort).toArray[Short]


  // Skewing factors for 2D  (TODO - do these need to change based on gradients size?
  private val F2 = 0.5 * (Math.sqrt(3.0) - 1.0)
  private val G2 = (3.0 - Math.sqrt(3.0)) / 6.0
  // Skewing factors for 3D
  private val F3 = 1.0 / 3.0
  private val G3 = 1.0 / 6.0


  // This method is a *lot* faster than using (int)Math.floor(x)
  private def fastfloor(x: Double): Int = {
    val xi = x.toInt
    if (x < xi) xi - 1 else xi
  }

  private def dot(g: Gradient, x: Double, y: Double): Double = g.x * x + g.y * y
  private def dot(g: Gradient, x: Double, y: Double, z: Double): Double = g.x * x + g.y * y + g.z * z

  val default = new SimplexNoise(grad3)

  /** Grabs simplex noise using built in gradients. */
  def noise(x: Double, y: Double): Double = default(x, y)

  /** Creates simplex noise using random grandients. */
  def random(rand: java.util.Random = new java.util.Random()): SimplexNoise = {
    def randInt: Int = rand.nextInt(2) - 1
    def randGrad = Gradient(randInt, randInt, randInt)
    new SimplexNoise((0 until 12).map(i => randGrad).toArray)
  }

  def apply(gradients: Array[Gradient]): SimplexNoise = new SimplexNoise(gradients)

}

class SimplexNoise(gradients: Array[Gradient]) {
  // To remove the need for index wrapping, double the permutation table length
  private val perm: Array[Short] = (0 until 512).map(i => (i.toShort & 255).toShort).toArray[Short]
  private val permMod12: Array[Short] = (0 until 512).map(i => (perm(i) % gradients.length).toShort).toArray[Short]

  import SimplexNoise._

  /** returns 2D simplex noise
    *
    * @param xin A value in [0.0, 1.0]  Can exceed bounds if repeated noise desired
    * @param yin A value in [0.0, 1.0] Can exceed bounds if repeated noise desired
    * @return A value in [-1.0, 1.0]
    */
  def apply(xin: Double, yin: Double): Double = {
    // Skew the input space to determine which simplex cell we're in
    val s = (xin + yin) * F2 // Hairy factor for 2D
    val i = fastfloor(xin + s)
    val j = fastfloor(yin + s)
    val t = (i + j) * G2
    val X0 = i - t // Unskew the cell origin back to (x,y) space
    val Y0 = j - t
    val x0 = xin - X0 // The x,y distances from the cell origin
    val y0 = yin - Y0
    // For the 2D case, the simplex shape is an equilateral triangle.
    // Determine which simplex we are in.
    val (i1, j1) = // Offsets for second (middle) corner of simplex in (i,j) coords
      if (x0 > y0) (1, 0) // lower triangle, XY order: (0,0)->(1,0)->(1,1)
      else (0, 1) // upper triangle, YX order: (0,0)->(0,1)->(1,1)
    // A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
    // a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where
    // c = (3-sqrt(3))/6
    val x1 = x0 - i1 + G2 // Offsets for middle corner in (x,y) unskewed coords
    val y1 = y0 - j1 + G2
    val x2 = x0 - 1.0 + 2.0 * G2 // Offsets for last corner in (x,y) unskewed coords
    val y2 = y0 - 1.0 + 2.0 * G2
    // Work out the hashed gradient indices of the three simplex corners
    val ii = i & 255
    val jj = j & 255
    val gi0 = permMod12(ii + perm(jj))
    val gi1 = permMod12(ii + i1 + perm(jj + j1))
    val gi2 = permMod12(ii + 1 + perm(jj + 1))
    // Calculate the contribution from the three corners
    var t0 = 0.5 - x0 * x0 - y0 * y0
    val n0 =
      if (t0 < 0) 0.0
      else {
        t0 *= t0
        t0 * t0 * dot(gradients(gi0), x0, y0) // (x,y) of grad3 used for 2D gradient
      }
    var t1 = 0.5 - x1 * x1 - y1 * y1
    val n1 =
      if (t1 < 0) 0.0
      else {
        t1 *= t1
        t1 * t1 * dot(gradients(gi1), x1, y1)
      }
    var t2 = 0.5 - x2 * x2 - y2 * y2
    val n2 =
      if (t2 < 0) 0.0
      else {
        t2 *= t2
        t2 * t2 * dot(gradients(gi2), x2, y2)
      }
    // Add contributions from each corner to get the final noise value.
    // The result is scaled to return values in the interval [-1,1].
    70.0 * (n0 + n1 + n2)
  }

  // 3D simplex noise
  def apply(xin: Double, yin: Double, zin: Double): Double = {
    // Skew the input space to determine which simplex cell we're in
    val s = (xin+yin+zin)*F3 // Very nice and simple skew factor for 3D
    val i = fastfloor(xin+s)
    val j = fastfloor(yin+s)
    val k = fastfloor(zin+s)
    val t = (i+j+k)*G3
    val X0 = i-t // Unskew the cell origin back to (x,y,z) space
    val Y0 = j-t
    val Z0 = k-t
    val x0 = xin-X0 // The x,y,z distances from the cell origin
    val y0 = yin-Y0
    val z0 = zin-Z0
    // For the 3D case, the simplex shape is a slightly irregular tetrahedron.
    // Determine which simplex we are in.
    val (i1, j1, k1, i2, j2, k2) =
      if(x0>=y0) {
        if(y0>=z0)
          (1, 0, 0, 1, 1, 0)  // X Y Z order
        else if(x0>=z0) (1, 0, 0, 1, 0, 1) // X Z Y order
        else (0, 0, 1, 1, 0, 1) // Z X Y order
      }
      else { // x0<y0
        if(y0<z0) (0, 0, 1, 0, 1, 1) // Z Y X order
        else if(x0<z0) (0, 1, 0, 0, 1, 1) // Y Z X order
        else (0, 1, 0, 1, 1, 0)  // Y X Z order
      }
    // A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z),
    // a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z), and
    // a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in (x,y,z), where
    // c = 1/6.
    val x1 = x0 - i1 + G3 // Offsets for second corner in (x,y,z) coords
    val y1 = y0 - j1 + G3
    val z1 = z0 - k1 + G3
    val x2 = x0 - i2 + 2.0*G3 // Offsets for third corner in (x,y,z) coords
    val y2 = y0 - j2 + 2.0*G3
    val z2 = z0 - k2 + 2.0*G3
    val x3 = x0 - 1.0 + 3.0*G3 // Offsets for last corner in (x,y,z) coords
    val y3 = y0 - 1.0 + 3.0*G3
    val z3 = z0 - 1.0 + 3.0*G3
    // Work out the hashed gradient indices of the four simplex corners
    val ii = i & 255
    val jj = j & 255
    val kk = k & 255
    val gi0 = permMod12(ii+perm(jj+perm(kk)))
    val gi1 = permMod12(ii+i1+perm(jj+j1+perm(kk+k1)))
    val gi2 = permMod12(ii+i2+perm(jj+j2+perm(kk+k2)))
    val gi3 = permMod12(ii+1+perm(jj+1+perm(kk+1)))
    // Calculate the contribution from the four corners
    var t0 = 0.6 - x0*x0 - y0*y0 - z0*z0
    val n0 = if(t0<0) 0.0
    else {
      t0 *= t0
      t0 * t0 * dot(grad3(gi0), x0, y0, z0)
    }
    var t1 = 0.6 - x1*x1 - y1*y1 - z1*z1
    val n1 = if(t1<0) 0.0
    else {
      t1 *= t1
      t1 * t1 * dot(gradients(gi1), x1, y1, z1)
    }
    var t2 = 0.6 - x2*x2 - y2*y2 - z2*z2
    val n2 = if(t2<0) 0.0
    else {
      t2 *= t2
      t2 * t2 * dot(gradients(gi2), x2, y2, z2)
    }
    var t3 = 0.6 - x3*x3 - y3*y3 - z3*z3
    val n3 = if(t3<0) 0.0
    else {
      t3 *= t3
      t3 * t3 * dot(gradients(gi3), x3, y3, z3)
    }
    // Add contributions from each corner to get the final noise value.
    // The result is scaled to stay just inside [-1,1]
    32.0*(n0 + n1 + n2 + n3)
  }
}



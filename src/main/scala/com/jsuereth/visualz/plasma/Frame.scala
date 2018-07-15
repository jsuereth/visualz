package com.jsuereth.visualz.plasma

import scala.reflect.ClassTag

/** A mutable frame cass that allows us to construct and manipulate values. */
class Frame[T : ClassTag](val width: Int, val height: Int) {
  private val storage = new Array[T](height*width)

  private def indexOf(x: Int, y: Int): Int = x + (y * width)

  def value(x: Int, y: Int): T = storage(indexOf(x,y))
  def write(x: Int, y: Int, value: T): Unit = storage(indexOf(x,y)) = value
  private def writeRaw(idx: Int, value: T): Unit = storage(idx) = value
  /** Normalize all values to a (0.0, 1.0) range. */
  def normalize(implicit n: Fractional[T]): Unit = {
    val min = storage.min
    val max = storage.max
    val diff = n.minus(max, min)
    def norm(in: T): T = n.div(n.minus(in, min), diff)
    for (i <- 0 until storage.length) {
      storage(i) = norm(storage(i))
    }
  }

  def map[U: ClassTag](f: T => U): Frame[U] = {
    val next = new Frame[U](width, height)
    var idx = 0
    while (idx < storage.length) {
      next.writeRaw(idx, f(storage(idx)))
      idx += 1
    }
    next
  }

  def mutMap(f: T => T): Frame[T] = {
    var idx = 0
    while (idx < storage.length) {
      storage(idx) = f(storage(idx))
      idx += 1
    }
    this
  }

  def maxValue(implicit o: Ordering[T]): T = storage.max
  def minValue(implicit o: Ordering[T]): T = storage.min
}

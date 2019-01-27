package parvisimulaatio

import scala.math._

// Points in a 2D space

case class Point2D(x:Double, y:Double) {

  def this() = this(0, 0)

  // Methods for calculations with another Point2D
  def +(p:Point2D) = Point2D(x + p.x, y + p.y)
  def -(p:Point2D) = Point2D(x - p.x, y - p.y)
  def *(p:Point2D) = Point2D(x * p.x, y * p.y)
  def /(p:Point2D) = Point2D(x / p.x, y / p.y)

  // Methods for calculations with a Double
  def +(d:Double) = Point2D(x + d, y + d)
  def -(d:Double) = Point2D(x - d, y - d)
  def *(d:Double) = Point2D(x * d, y * d)
  def /(d:Double) = Point2D(x / d, y / d)

  // Used to calculate the hypotenuse
  def distanceToSquared(p:Point2D): Double = {
    val dx = x - p.x
    val dy = y - p.y
    (dx * dx) + (dy * dy)
  }
  
  // Calculating the length between two points (= hypotenuse)
  def distanceTo(p:Point2D): Double = sqrt(distanceToSquared(p))
  
  def length: Double = distanceTo(Point2D.ORIGIN)
  
  override def toString = "Point2D(" + x + ", " + y + ")"

}

object Point2D {
  val _ORIGIN = new Point2D(0, 0)
  def ORIGIN =_ORIGIN
}
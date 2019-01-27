package parvisimulaatio

// Class for 2D vectors not in use at the moment

case class Vector2D(x: Double, y: Double) {
  
  val length = math.hypot(x, y)
  val angle  = math.atan2(y, x)
  
  
  //returns a sum of two vectors
  def + (other: Vector2D) = {
    Vector2D(x + other.x, y + other.y)    
  }
  
  //returns a sum of this vector and a Double
  def + (other: Double) = {
    Vector2D(x + other, y + other)    
  }
  
}
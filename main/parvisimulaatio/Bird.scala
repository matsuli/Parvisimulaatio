package parvisimulaatio

import java.awt.Graphics2D
import java.awt.geom.{GeneralPath, Path2D}
import scala.math._

class Bird(var mass: Double, var velocity: Point2D, var position: Point2D) {
  
  // Different weights used to calculate the velocity for a bird
  var cohesionWeight: Int = 120
  var avoidanceWeight: Int = 30
  var consistencyWeight: Int = 2000
  var momentumWeight: Int = 150
  
  // Calculates the orientation of the bird, used in rotating the bird according to its movement
  def orientation: Double = {
    if (velocity.x < 0 && velocity.y < 0) -atan(velocity.x/(velocity.y)) - math.Pi/2
    else if (velocity.x < 0 && velocity.y > 0) -atan(velocity.x/(velocity.y)) + math.Pi/2
    else atan(velocity.y/(velocity.x))
  }
  
  // The maximum speed a bird can have
  var maxSpeed: Double = 0.7
  
  // The flock the bird belongs to
  var flock: Flock = _
  def getFlock: Flock = flock
  
  // Gets called by Flock everytime a bird is added
  def setFlock(f: Flock): Unit = { 
    flock = f 
  }
  
  // Buffer with the closest birds to this one. Improved performance with many birds
  var closestBirds: Array[Bird] = Array()
  
  
  // The shape of the birds
  // Three points on the periphery of a circle
  val shape: GeneralPath = {
    val angles  = Array(math.Pi/2, math.Pi*7/6, math.Pi*11/6)
    val radii   = Array(10, 5, 5)
    
    val xCoords = for {
      (r, theta) <- (radii zip angles)
    } yield r * math.cos(theta)

    val yCoords = for {
      (r, theta) <- (radii zip angles)
    } yield r * math.sin(theta)

    val polyline = new GeneralPath(Path2D.WIND_NON_ZERO, 10)  
    polyline.moveTo(xCoords.head, yCoords.head)
    
    for ((x,y) <- (xCoords.tail zip yCoords.tail))
      polyline.lineTo(x,y)
      
    polyline
  }
  
  // Calculates the new velocity for the bird. Takes into account the weights and speed limit
  def newVelocity: Point2D = {
    
    // Using only the closest birds
    closestBirds = flock.closestToThis(this)
    
    // Calculate new velocity taking into account the weights
    var distance = {
      moveTowardsFlockCenter * cohesionWeight +
      avoidOthers * avoidanceWeight +
      matchOthersVelocities * consistencyWeight +
      velocity * momentumWeight * this.mass
    }
    
    // Limit the velocity
    distance /= (sqrt(pow(distance.x, 2.0) + pow(distance.y, 2.0))/maxSpeed)
    
    distance
  }
  
  // Move method for the bird. Adds the new velocity to the old velocity and checks if the bird is inside the window
  def move() = {
    // No need to calculate new velocity if only on bird is present in the simulation
    if (flock.birds.length == 1) {
      velocity /= velocity.length/maxSpeed
      position += velocity
    } else {
      velocity += newVelocity
      velocity /= velocity.length/maxSpeed
      position += velocity
    }
    // Keep the birds inside the window
    position = bound(900, 880)
  }
  
  // Moves towards the center of the flock
  def moveTowardsFlockCenter: Point2D = {
    var p = new Point2D()
    for (b <- closestBirds if b != this) {
      p += b.position
    }
    ((p / (closestBirds.length - 1)) - position) / 50
  }
  
  // Avoid other birds
  def avoidOthers: Point2D = {
    var c = new Point2D()
    for (b <- closestBirds if b != this) {
      val otherPos = b.position
      val distanceToOther = this.position.distanceTo(otherPos)
      if (distanceToOther < 50){
        c = ((c + position - otherPos) * 2000)/(distanceToOther*distanceToOther)
      }
    }
    c
  }
  
  // Matches the others velocities
  def matchOthersVelocities: Point2D = {
    var vel = new Point2D()
    for (b <- closestBirds if b != this) {
      vel += b.velocity
    }
    vel /= flock.size - 1
    vel
  }
  
  // Keep the birds inside the window by teleporting them to the other side window when crossing the edge
  def bound(xBound: Int, yBound: Int): Point2D = {
    val x = position.x
    val y = position.y
    
    val newX = {
      if (x >= xBound) x - xBound
      else if (x < 0) x + xBound
      else x
    }
    val newY = {
      if (y >= yBound) y - yBound
      else if (y < 0) y + yBound
      else y
    }
    if (newX != x || newY != y)
      Point2D(newX, newY)
    else
      position
  }
  
  // Draws and rotates the bird
  def draw(g: Graphics2D) = {
    val oldTransform = g.getTransform()
    
    val oldX = position.x
    val oldY = position.y
    
    g.translate(oldX, oldY)
    g.rotate(orientation - (math.Pi/2))

    g.fill(shape)
    g.rotate(-(orientation - (math.Pi/2)))
    g.translate(-oldX, -oldY)
    
    g.setTransform(oldTransform)
  }
  
  
}
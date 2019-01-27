package parvisimulaatio

import scala.collection.mutable.Buffer
import java.awt.Graphics2D
import scala.math._

  // Class used to control all the birds, also contains useful method closestToThis
class Flock {
    
  // All the birds in the flock is stored here
  val birds: Buffer[Bird] = Buffer[Bird]()
  
  def size: Int = birds.length
  
  // Adds x amount of birds to the simulation
  def addBird(b: Bird) = {
    birds += b
    b.setFlock(this)
  }
  
  // Adds a bird at the coordinates (x, y)
  def addBirdAt(x: Int, y: Int) = {
    val rand = new java.util.Random()
      val speed = Point2D(rand.nextDouble, rand.nextDouble)
      val pos = Point2D(x, y)
      addBird(new Bird(100, speed, pos))
      println(size)
  }
  
  
  // Draws all the birds
  def draw(g: Graphics2D) = birds.foreach(_.draw(g))
  
  // Moves all the birds one step
  def step() = birds.foreach(_.move())
  
  // Returns the closest birds to the calling bird
  // Used if there are more than 50 birds
  // Returns the sqrt(amountOfBirds)*2 closest birds
  def closestToThis(bird: Bird): Array[Bird] = {
    val birdsWithDistances = birds.toArray.map(x => (x, x.position.distanceTo(bird.position)))
    
    if(birds.size > 50) {
      birdsWithDistances.sortBy(_._2).take((sqrt(birds.size)*2).toInt).map(_._1)
    } else birds.toArray
  }
}
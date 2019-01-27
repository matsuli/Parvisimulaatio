package parvisimulaatio

import scala.swing._
import java.awt.{Color, BasicStroke, Graphics2D, RenderingHints}
import java.awt.event.ActionListener
import scala.swing.event.Key
import scala.math.BigDecimal

// The main simulation
class Simulation(amountOfBirds: Int) extends SimpleSwingApplication {
  
  val width      = 900
  val height     = 900
  val fullHeight = 910
  
  val flock = new Flock
  
  def top = new MainFrame {
    
    title     = "Simulation"
    resizable = false
    
    minimumSize   = new Dimension(width,fullHeight)
    preferredSize = new Dimension(width,fullHeight)
    maximumSize   = new Dimension(width,fullHeight)
    
    val rand = new java.util.Random()
    
    // Adds the selected amount of birds to the simulation with random positions and velocities
    for (i <- 1 to amountOfBirds) {
      val speed = Point2D(rand.nextDouble * 10, rand.nextDouble * 10)
      val pos = Point2D(rand.nextInt(900), rand.nextInt(900))
      flock.addBird(new Bird(100, speed, pos))
    }
    
    
    val arena = new BoxPanel(Orientation.Vertical) {
     focusable = true
     contents += new Panel {
      override def paintComponent(g: Graphics2D) = {
        
        g.setColor(new Color(255, 255, 255))
        g.fillRect(0, 0, width, fullHeight)
        
        // Antialiasing
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)          

        // Change color and draw the birds
        g.setColor(new Color(0, 240, 240))
        flock.draw(g) 
      }
     }
     // The text showing the settings
     val textLabel = new Label {
       text = "Use arrowkeys to change the weights. Press Enter to reset your changes! " +
              "Left click to add birds and right click to remove them. Esc to quit"
     }
     contents += textLabel
    }
    
    // Adds arena to our MainFrame
    contents = arena
    
    // Listen to our mouse clicks and key presses
    listenTo(arena.mouse.clicks)
    listenTo(arena.keys)
    
    // variable used to determine which weight is selected
    //    1 => cohesionWeight
    //    2 => avoidanceWeight
    //    3 => consistencyWeight
    //    4 => momentumWeight
    //    5 => maxSpeed
    var weightSelected: Int = 1
    
    // returns the value of the selected weight
    def weightValue: String = {
      if (!flock.birds.isEmpty) {
        weightSelected match {
          case 1 => flock.birds(0).cohesionWeight.toString()
          case 2 => flock.birds(0).avoidanceWeight.toString()
          case 3 => flock.birds(0).consistencyWeight.toString()
          case 4 => flock.birds(0).momentumWeight.toString()
          case 5 => "%.1f".format(flock.birds(0).maxSpeed)
          case _ => ""
        }
      } else ""
    }
    
    // Updates the text in the label showing what weight is selected
    def textUpdate: Unit = {
      arena.textLabel.text = weightSelected match {
        case 1 => "Cohesion " + weightValue
        case 2 => "Avoidance " + weightValue
        case 3 => "Consistency " + weightValue
        case 4 => "Momentum " + weightValue
        case 5 => "Speed " + weightValue
      }
    }
    
    reactions += {
        // Pressing left mousekey adds a bird at mouselocation and right click removes a random bird
        case scala.swing.event.MousePressed(_, point, a, _, _) => {
          if (a == 1024) flock.addBirdAt(point.x, point.y)
          if (a == 4096 && flock.birds.size > 0) flock.birds.remove(0)
        }
        // Up and down keys scroll between the different weights and update the label text
        case scala.swing.event.KeyPressed(_, Key.Up, _, _) => {
          if (weightSelected == 5) weightSelected = 1
          else weightSelected += 1
          textUpdate
        }
        case scala.swing.event.KeyPressed(_, Key.Down, _, _) => {
          if (weightSelected == 1) weightSelected = 5
          else weightSelected -= 1
          textUpdate
        }
        // Increases the value of the selected weight and updates the label text
        case scala.swing.event.KeyPressed(_, Key.Right, _, _) => {
          weightSelected match {
            case 1 => flock.birds.foreach(_.cohesionWeight += 10)
            case 2 => flock.birds.foreach(_.avoidanceWeight += 10)
            case 3 => flock.birds.foreach(_.consistencyWeight += 10)
            case 4 => flock.birds.foreach(_.momentumWeight += 10)
            case 5 => flock.birds.foreach((_.maxSpeed += 0.1))
          }
          textUpdate
        }
        // Decreases the value of the selected weight and updates the label text
        case scala.swing.event.KeyPressed(_, Key.Left, _, _) => {
          weightSelected match {
            case 1 => if (flock.birds(0).cohesionWeight > 11) flock.birds.foreach(_.cohesionWeight -= 10)
            case 2 => if (flock.birds(0).avoidanceWeight > 11) flock.birds.foreach(_.avoidanceWeight -= 10)
            case 3 => if (flock.birds(0).consistencyWeight > 11) flock.birds.foreach(_.consistencyWeight -= 10)
            case 4 => if (flock.birds(0).momentumWeight > 11) flock.birds.foreach(_.momentumWeight -= 10)
            case 5 => if (flock.birds(0).maxSpeed > 0.2) flock.birds.foreach(_.maxSpeed -= 0.1)
          }
          textUpdate
        }
        // Resets the changes the user has made to the weights
        case scala.swing.event.KeyPressed(_, Key.Enter, _, _) => {
          flock.birds.foreach(_.cohesionWeight = 100)
          flock.birds.foreach(_.avoidanceWeight = 5)
          flock.birds.foreach(_.consistencyWeight = 2000)
          flock.birds.foreach(_.momentumWeight = 50)
          flock.birds.foreach(_.maxSpeed = 0.7)
          textUpdate
        }
        // Pressing escape quits the program
        case scala.swing.event.KeyPressed(_, Key.Escape, _, _) => {
          quit()
        }
    } 
    
    // listener used by the timer to calling step to the flock and repainting the arena
    val listener = new ActionListener(){
      def actionPerformed(e : java.awt.event.ActionEvent) = {
        flock.step()
        arena.repaint() 
      }  
    }
    
    // Timer sends an ActionEvent to ActionListener every 6ms
    // making the space take a step and redrawing the window
    val timer = new javax.swing.Timer(6, listener)
    timer.start()
    
  }
}
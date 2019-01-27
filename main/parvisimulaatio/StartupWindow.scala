package parvisimulaatio

import scala.swing._
import java.awt.{Color, BasicStroke, Graphics2D, RenderingHints}
import java.awt.event._
import scala.swing.event.MouseClicked
import scala.swing.event.ButtonClicked
import scala.swing.BorderPanel.Position._

// The settings window

object StartupWindow extends SimpleSwingApplication {
  
  val width  = 400
  val height = 200
  
  
  def top = new MainFrame {
    
    title     = "Settings"
    preferredSize = new Dimension(width, height)
    centerOnScreen()
    
    // Button to start the simulation
    val Proceedbutton = new Button("Proceed") {
      horizontalAlignment = Alignment.Center
    }
    
    // Slider used to choose amount of birds
    val birdAmountSlider = new Slider {
      min = 0
      majorTickSpacing = 1
      minorTickSpacing = 1
      max = 10
      value = 5
      paintTicks = true
      paintLabels = true
      labels = Map(0 -> new Label("0"), 5 -> new Label("5"), 10 -> new Label("10"))
    }
    
    val numberBirds = new Label("Choose number of birds") 

    // The layout for the settings window
    contents = new BoxPanel(Orientation.Vertical) {
    contents += Swing.VStrut(20)
    
    contents += new BorderPanel {
      layout(numberBirds) = North
      layout(birdAmountSlider) = Center
    }
    
    contents += new FlowPanel {
      contents += Proceedbutton
      contents += Button("Close") { sys.exit(0) }
    }
    
    contents += Swing.VStrut(20)
  }
    
    listenTo(this.Proceedbutton)
    
    // If button "Proceed" is pressed this window will close and the simulation will start
    reactions += {
      case ButtonClicked(_)  => {
        this.dispose()
        new Simulation(birdAmountSlider.value).top.visible = true
      }
    }
  }
}
import scalafx.Includes._
import scala.collection.mutable.ListBuffer
import scalafx.scene.layout.Pane
import scalafx.scene.Group
import scalafx.scene.input.KeyEvent
import scalafx.animation.AnimationTimer
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.scene.Scene



abstract class Fruit extends Circle {
  val dt = 0.045
  val gravity = 850
  var falling = false
  centerX = 200

  def step(): Unit

  def moveLeft(): Unit = {
    if(!falling) centerX = centerX.value - 10.0
  }

  def moveRight(): Unit = {
    if(!falling) centerX = centerX.value + 10.0
  }

  def toggleFalling() = {
    falling = !falling
  }
}

case class Cherry() extends Fruit {
  var VelocityY = 0.0
  val radiusInt = 15

  def step(): Unit = {
    if (centerY.value + radiusInt >= 500) {
      centerY = 500 - radiusInt
      VelocityY = 0.0
    } else {
      if(falling) {
        VelocityY += gravity * dt
        centerY = VelocityY * dt
      } else {
        VelocityY = 0.0
      }
    }
  }

  // Circle Stuff
  centerY = 15
  radius = radiusInt
  fill = Color.DarkRed
}

object SuikaGame extends JFXApp3 {
  // Collection to hold multiple fruits
  val fruits: ListBuffer[Fruit] = ListBuffer()

  override def start(): Unit = {
    // Initialize the root pane
    val rootPane = new Pane {
      focusTraversable = true // Ensure pane can receive focus
    }

    // Create the scene with a background color
    stage = new PrimaryStage {
      title = "Suika Game"
      width = 800
      height = 600
      scene = new Scene {
        fill = Color.AliceBlue
        content = rootPane

        onKeyPressed = (ke: KeyEvent) => {
          ke.code match {
            case scalafx.scene.input.KeyCode.Left =>
              fruits.last.moveLeft()
            case scalafx.scene.input.KeyCode.Right =>
              fruits.last.moveRight()
            case scalafx.scene.input.KeyCode.Space =>
              fruits.last.falling = true
              addFruit(Cherry())
            case _ =>
          }
        }
      }
    }

    def addFruit(fruit: Fruit): Unit = {
      fruits += fruit
      rootPane.children += fruit
    }

    addFruit(Cherry())

    val timer = AnimationTimer { now =>
      fruits.foreach(_.step())
    }

    timer.start()
  }
}

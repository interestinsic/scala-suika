import scalafx.Includes._
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import scalafx.scene.Group
import scalafx.animation.AnimationTimer
import scalafx.scene.input.KeyEvent


class Fruit extends Circle {
  val dt = 0.045
  val gravity = 850
  var falling = false
  centerX = 200

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
  override def start(): Unit = {
    val fruit = new Cherry
    val rootGroup = new Group(fruit)

    stage = new PrimaryStage {
      title = "Suika Game"
      scene = new Scene(500, 500) {
        content = rootGroup

        onKeyPressed = (ke: KeyEvent) => {
          ke.code match {
            case scalafx.scene.input.KeyCode.Left =>
              fruit.moveLeft()
            case scalafx.scene.input.KeyCode.Right =>
              fruit.moveRight()
            case scalafx.scene.input.KeyCode.Space =>
              fruit.falling = true
            case _ => // Do nothing for other keys
          }
        }

      }
    }

    val timer = AnimationTimer { now =>
      // Call step to simulate the gravity effect on the fruit
      fruit.step()
    }

    timer.start()
  }
}

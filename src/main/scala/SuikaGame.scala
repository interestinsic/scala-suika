import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import scalafx.scene.Group
import scalafx.animation.AnimationTimer
import scalafx.scene.input.KeyCode
import scalafx.scene.input.KeyEvent


class Fruit extends Circle {
  val dt = 0.045
  val gravity = 850
  centerX = 200

  def moveLeft(): Unit = {
    centerX = centerX.value - 5.0
  }

  def moveRight(): Unit = {
    centerX = centerX.value + 5.0
  }
}

case class Cherry() extends Fruit {
  var VelocityY = 0.0
  val radiusInt = 15

  def step(): Unit = {
    if (centerY.value + radiusInt >= 500 /* Detect Collision */) {
      centerY = 500 - radiusInt
      VelocityY = 0.0
    } else {
      VelocityY += gravity * dt
      centerY = VelocityY * dt
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


      }
    }

    val timer = AnimationTimer { now =>
      // Call step to simulate the gravity effect on the fruit
      fruit.step()
    }

    timer.start()
  }
}

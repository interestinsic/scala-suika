import math._
import scala.collection.mutable.ListBuffer
import scalafx.Includes._
import scalafx.scene.layout.Pane
import scalafx.scene.Group
import scalafx.scene.input.KeyEvent
import scalafx.animation.AnimationTimer
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.scene.Scene

object CollisionUtils {
  def isColliding(fruit1: Fruit, fruit2: Fruit): Boolean = {
    val dx = fruit1.centerX.value - fruit2.centerX.value
    val dy = fruit1.centerY.value - fruit2.centerY.value
    val distance = sqrt(dx * dx + dy * dy)
    distance < (fruit1.radius.value + fruit2.radius.value)
  }
}


abstract class Fruit extends Circle {
  val dt: Double = 0.016 // Approximately 60 FPS

  val gravity: Double = 1000.0
  var falling: Boolean = false

  var velocityX: Double = 0.0
  var velocityY: Double = 0.0

  centerX = 200.0
  centerY = 15.0

  def step(): Unit

  def moveLeft(): Unit = {
    if(!falling) centerX = centerX.value - 10.0
  }

  def moveRight(): Unit = {
    if(!falling) centerX = centerX.value + 10.0
  }

  def stopMovement(): Unit = {
    velocityX = 0.0
  }

  def toggleFalling(): Unit = {
    falling = !falling
  }
}

class Cherry extends Fruit {
  val radiusInt: Double = 15.0

  // Initialize Cherry Properties
  radius = radiusInt
  fill = Color.DarkRed

  // Override Step Method
  override def step(): Unit = {
    if (falling) {
      velocityY += gravity * dt
    } else {
      velocityY = 0.0
    }

    centerX = centerX.value + velocityX * dt
    centerY = centerY.value + velocityY * dt

    if (centerY.value + radius.value >= 500) {
      centerY = 500 - radius.value
      velocityY = 0.0
      falling = false
    }

    if (centerX.value - radius.value <= 0) {
      centerX = radius.value
      velocityX = -velocityX * 0.5 // Reverse and dampen velocity
    } else if (centerX.value + radius.value >= 500) {
      centerX = 500 - radius.value
      velocityX = -velocityX * 0.5 // Reverse and dampen velocity
    }
  }
}


object SuikaGame extends JFXApp3 {
  val fruits: ListBuffer[Fruit] = ListBuffer()

  def handleCollision(fruitA: Fruit, fruitB: Fruit): Unit = {
    val tempVX = fruitA.velocityX
    fruitA.velocityX = fruitB.velocityX
    fruitB.velocityX = tempVX

    val tempVY = fruitA.velocityY
    fruitA.velocityY = fruitB.velocityY
    fruitB.velocityY = tempVY

    val overlap = (fruitA.radius.value + fruitB.radius.value) - distance(fruitA, fruitB)
    if (overlap > 0) {
      val angle = atan2(fruitB.centerY.value - fruitA.centerY.value, fruitB.centerX.value - fruitA.centerX.value)
      val moveA = overlap / 2 * cos(angle)
      val moveB = overlap / 2 * sin(angle)
      fruitA.centerX = fruitA.centerX.value - moveA
      fruitA.centerY = fruitA.centerY.value - moveB
      fruitB.centerX = fruitB.centerX.value + moveA
      fruitB.centerY = fruitB.centerY.value + moveB
    }
  }

  def distance(fruitA: Fruit, fruitB: Fruit): Double = {
    val dx = fruitA.centerX.value - fruitB.centerX.value
    val dy = fruitA.centerY.value - fruitB.centerY.value
    sqrt(dx * dx + dy * dy)
  }


  override def start(): Unit = {
    val rootPane = new Pane {
      focusTraversable = true
    }

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
              addFruit(new Cherry())
            case _ =>
          }
        }
      }
    }

    def addFruit(fruit: Fruit): Unit = {
      fruits += fruit
      rootPane.children += fruit
    }

    addFruit(new Cherry())

    val timer = AnimationTimer { now =>
      fruits.foreach(_.step())

      for {
        i <- fruits.indices
        j <- (i + 1) until fruits.length
      }{
        val fruitA = fruits(i)
        val fruitB = fruits(j)
        if(CollisionUtils.isColliding(fruitA, fruitB)) {
          handleCollision(fruitA, fruitB)
        }
      }
    }

    timer.start()
  }
}

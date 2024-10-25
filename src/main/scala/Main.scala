import math._
import scala.collection.mutable.ListBuffer
import scala.util.Random
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
import scalafx.animation.PauseTransition
import scalafx.util.Duration


object FruitEvolution {
  val evolutionMap: Map[Class[_ <: Fruit], () => Fruit] = Map(
    classOf[Cherry]      -> (() => new Strawberry()),
    classOf[Strawberry] -> (() => new Grape()),
    classOf[Grape]       -> (() => new Dekopon()),
    classOf[Dekopon]     -> (() => new Orange()),
    classOf[Orange]      -> (() => new Apple()),
    classOf[Apple]       -> (() => new Pear()),
    classOf[Pear]        -> (() => new Peach()),
    classOf[Peach]       -> (() => new Pineapple()),
    classOf[Pineapple]   -> (() => new Melon()),
    classOf[Melon]       -> (() => new Watermelon())
  )

  // Method to get the next fruit; returns None if there's no further evolution
  def getNextFruit(current: Fruit): Option[Fruit] = {
    evolutionMap.get(current.getClass).map(createFruit => createFruit())
  }
}

object CollisionUtils {

  def distance(fruitA: Fruit, fruitB: Fruit): Double = {
    val dx = fruitA.centerX.value - fruitB.centerX.value
    val dy = fruitA.centerY.value - fruitB.centerY.value
    sqrt(dx * dx + dy * dy)
  }

  def isColliding(fruit1: Fruit, fruit2: Fruit): Boolean = {
    val dx = fruit1.centerX.value - fruit2.centerX.value
    val dy = fruit1.centerY.value - fruit2.centerY.value
    val distance = sqrt(dx * dx + dy * dy)
    distance < (fruit1.radius.value + fruit2.radius.value)
  }

  def handleCollision(fruitA: Fruit, fruitB: Fruit, fruits: ListBuffer[Fruit], rootPane: Pane): Boolean = {
    if (fruitA.getClass == fruitB.getClass) {
      // Attempt to evolve the fruit
      FruitEvolution.getNextFruit(fruitA) match {
        case Some(newFruit) =>
          // Calculate the new position as the average of both fruits
          newFruit.centerX = (fruitA.centerX.value + fruitB.centerX.value) / 2
          newFruit.centerY = (fruitA.centerY.value + fruitB.centerY.value) / 2

          // Remove the old fruits from the game
          fruits -= fruitA
          fruits -= fruitB
          rootPane.children -= fruitA
          rootPane.children -= fruitB

          // Add the new evolved fruit to the game (not controlled)
          fruits += newFruit
          rootPane.children += newFruit

          true

        case None =>
          swapVelocities(fruitA, fruitB)
          resolveOverlap(fruitA, fruitB)
          false
      }
    } else {
      swapVelocities(fruitA, fruitB)
      resolveOverlap(fruitA, fruitB)
      false
    }
  }

  private def swapVelocities(fruitA: Fruit, fruitB: Fruit): Unit = {
    val tempVX = fruitA.velocityX
    fruitA.velocityX = fruitB.velocityX
    fruitB.velocityX = tempVX

    val tempVY = fruitA.velocityY
    fruitA.velocityY = fruitB.velocityY
    fruitB.velocityY = tempVY
  }

  private def resolveOverlap(fruitA: Fruit, fruitB: Fruit): Unit = {
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
}

abstract class Fruit(radiusInt: Int) extends Circle {
  radius = radiusInt

  val dt: Double = 0.016 // Approximately 60 FPS

  val gravity: Double = 1000.0
  var falling: Boolean = false

  var velocityX: Double = 0.0
  var velocityY: Double = 0.0

  val rand = new Random()

  centerX = 60 + rand.nextInt(280)
  centerY = 15.0

  def moveLeft(): Unit = {
    if(!falling) centerX = centerX.value - 10.0
  }

  def moveRight(): Unit = {
    if(!falling) centerX = centerX.value + 10.0
  }

  def step(): Unit = {
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
    } else if (centerX.value + radius.value >= 500) {
      centerX = 500 - radius.value
    }
  }
}

class Cherry extends Fruit(radiusInt = 10) {
  fill = Color.rgb(236, 55, 93)
}

class Strawberry extends Fruit(radiusInt = 18){
  fill = Color.rgb(230, 93, 50)
}

class Grape extends Fruit(radiusInt = 28){
  fill = Color.rgb(223, 120, 248)
}

class Dekopon extends Fruit(radiusInt = 38){
  fill = Color.rgb(255, 166, 57)
}

class Orange extends Fruit(radiusInt = 45){
  fill = Color.rgb(228, 139, 84)
}

class Apple extends Fruit(radiusInt = 58){
  fill = Color.rgb(251, 63, 75)
}

class Pear extends Fruit(radiusInt = 65) {
  fill = Color.rgb(235, 215, 73)
}

class Peach extends Fruit(radiusInt = 78){
  fill = Color.rgb(251, 152, 245)
}

class Pineapple extends Fruit(radiusInt = 88){
  fill = Color.rgb(242, 236, 20, 255)
}

class Melon extends Fruit(radiusInt = 100){
  fill = Color.rgb(197, 255, 91)
}

class Watermelon extends Fruit(radiusInt = 110){
  fill = Color.rgb(0, 153, 0)
}

object SuikaGame extends JFXApp3 {
  val fruits: ListBuffer[Fruit] = ListBuffer()
  var controlledFruit: Option[Fruit] = None
  var spawnCooldown: Boolean = false

  override def start(): Unit = {
    val rootPane = new Pane {
      focusTraversable = true
    }

    stage = new PrimaryStage {
      title = "Suika Game"
      width = 500
      height = 500
      scene = new Scene {
        fill = Color.AliceBlue
        content = rootPane

        onKeyPressed = (ke: KeyEvent) => {
          controlledFruit match {
            case Some(fruit) =>
              ke.code match {
                case scalafx.scene.input.KeyCode.Left =>
                  fruit.moveLeft()
                case scalafx.scene.input.KeyCode.Right =>
                  fruit.moveRight()
                case scalafx.scene.input.KeyCode.Space =>
                  fruit.falling = true
                  if(!spawnCooldown) {
                    spawnCooldown = true
                    val pause = new PauseTransition(Duration(1000)) // 1000 milliseconds = 1 second
                    pause.onFinished = (_: javafx.event.ActionEvent) => {
                      addFruit(new Cherry(), isControlled = true)
                      spawnCooldown = false
                    }
                    pause.play()
                  }
                case _ =>
              }
            case None =>
          }
        }
      }
    }

    def addFruit(fruit: Fruit, isControlled: Boolean = false): Unit = {
      fruits += fruit
      rootPane.children += fruit
      if (isControlled) {
        controlledFruit = Some(fruit)
      }
    }

    addFruit(new Cherry(), isControlled = true)

    val timer = AnimationTimer { now =>
      fruits.foreach(_.step())

      var combinationOccurred = false

      for {
        i <- fruits.indices
        j <- (i + 1) until fruits.length
      } {
        val fruitA = fruits(i)
        val fruitB = fruits(j)
        if (CollisionUtils.isColliding(fruitA, fruitB)) {
          if (CollisionUtils.handleCollision(fruitA, fruitB, fruits, rootPane)) {
            combinationOccurred = true
          }
        }
      }

    }

    timer.start()
  }
}

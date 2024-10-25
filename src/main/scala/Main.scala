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
    classOf[Melon]       -> (() => new Watermelon()),
  )

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

  def handleCollision(fruitA: Fruit, fruitB: Fruit, fruits: ListBuffer[Fruit], rootPane: Pane): Unit = {
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

          // Add the new evolved fruit to the game
          fruits += newFruit
          rootPane.children += newFruit

        case None =>
          // No further evolution; handle collision normally
          swapVelocities(fruitA, fruitB)
          resolveOverlap(fruitA, fruitB)
      }
    } else {
      // Different types; handle collision normally
      swapVelocities(fruitA, fruitB)
      resolveOverlap(fruitA, fruitB)
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


case class Fruit(radiusInt: Int) extends Circle {

  radius = radiusInt
  val dt: Double = 0.016 // Approximately 60 FPS

  val gravity: Double = 1000.0
  var falling: Boolean = false

  var velocityX: Double = 0.0
  var velocityY: Double = 0.0

  centerX = 200.0
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
          CollisionUtils.handleCollision(fruitA, fruitB, fruits, rootPane)
        }
      }
    }

    timer.start()
  }
}

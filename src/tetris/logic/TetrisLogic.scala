package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import sun.font.TrueTypeFont
import tetris.logic.TetrisLogic._

import scala.collection.immutable.Seq
import scala.runtime.Nothing$

class TetrisLogic(val randomGen: RandomGenerator,
                  val gridDims : Dimensions,
                  val initialBoard: Seq[Seq[CellType]]) {

  def this(random: RandomGenerator, gridDims : Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))

  val centerX: Int = {
    if (gridDims.width % 2 != 0) gridDims.width / 2 + 1
    else gridDims.width / 2
  }

  def spawnTetromino(): Tetromino = {

    val randomNum = randomGen.randomInt(7)
    val currTetromino: Tetromino = randomNum match {
    case 0 => Tetromino.I(centerX)
    case 1 => Tetromino.J(centerX)
    case 2 => Tetromino.L(centerX)
    case 3 => Tetromino.O(centerX)
    case 4 => Tetromino.S(centerX)
    case 5 => Tetromino.T(centerX)
    case 6 => Tetromino.Z(centerX)
  }
    currTetromino
  }

  var frame = Frame(spawnTetromino(), initialBoard)

  def placeTetromino(board: Seq[Seq[CellType]], tetromino: Tetromino, cellType: CellType): Seq[Seq[CellType]] = {
    tetromino.blocks.foldLeft(board) { (updatedBoard, point) =>
      updatedBoard.updated(point.y + tetromino.center.y, updatedBoard(point.y + tetromino.center.y).updated(point.x + tetromino.center.x, cellType))
    }
  }

  def freeUnderneath(): Boolean = {
    frame.tetromino.blocks.forall{p =>
      val nextPoint = p + frame.tetromino.center + Point(0, 1)
      if(nextPoint.y < gridDims.height){
        frame.board(nextPoint.y)(nextPoint.x) == Empty
      }
      else false
    }
  }

  def deleteRows(board: Seq[Seq[CellType]]): Seq[Seq[CellType]] = {
    val nonFullRows = board.filter(row => row.contains(Empty))
    val rowsDeleted = gridDims.height - nonFullRows.length
    val emptyRows = Seq.fill(rowsDeleted)(Seq.fill(gridDims.width)(Empty))
    emptyRows ++ nonFullRows
  }

  def validSpawn(board: Seq[Seq[CellType]], tetromino: Tetromino): Boolean = {
    tetromino.blocks.forall{p =>
      val point = p + tetromino.center
      board(point.y)(point.x) == Empty
    }
  }

  def rotateLeft(): Unit = {
    val rotatedTetromino = frame.tetromino.rotateLeft()
    val shiftedBlocks: List[Point] = rotatedTetromino.absoluteBlocks()
    if(shiftedBlocks.forall(b => b.x >= 0 && b.x < gridDims.width && b.y < gridDims.height) && shiftedBlocks.forall(b => frame.board(b.y)(b.x) == Empty))
      frame = frame.copy(tetromino = rotatedTetromino)
  }

  def rotateRight(): Unit = {
    val rotatedTetromino = frame.tetromino.rotateRight()
    val shiftedBlocks: List[Point] = rotatedTetromino.absoluteBlocks()
    if(shiftedBlocks.forall(b => b.x >= 0 && b.x < gridDims.width && b.y < gridDims.height) && shiftedBlocks.forall(b => frame.board(b.y)(b.x) == Empty))
      frame = frame.copy(tetromino = rotatedTetromino)
  }

  def moveLeft(): Unit = {
    val shiftedBlocks: List[Point] = frame.tetromino.absoluteBlocks(Point(-1,0))
    if(shiftedBlocks.forall(b => b.x >= 0) && shiftedBlocks.forall(b => frame.board(b.y)(b.x) == Empty))
      frame = frame.copy(tetromino = frame.tetromino.moveLeft())
  }

  def moveRight(): Unit = {
    val shiftedBlocks: List[Point] = frame.tetromino.absoluteBlocks(Point(1,0))
    if(shiftedBlocks.forall(b => b.x < gridDims.width) && shiftedBlocks.forall(b => frame.board(b.y)(b.x) == Empty))
      frame = frame.copy(tetromino = frame.tetromino.moveRight())
  }

  def moveDown(): Unit = { 
    if (freeUnderneath()) frame = frame.copy(tetromino = frame.tetromino.moveDown())
    else {
      val newBoard = placeTetromino(frame.board, frame.tetromino, frame.tetromino.cellType)
      frame = frame.copy(board = newBoard)
      frame = frame.copy(board = deleteRows(frame.board))

      val newTetromino = spawnTetromino()
      frame = frame.copy(tetromino = newTetromino)
      if (!validSpawn(frame.board, newTetromino)){
        frame = frame.copy(gameOver = true)
      }
    }
  }

  def doHardDrop(): Unit = {
    while(freeUnderneath()) frame = frame.copy(tetromino = frame.tetromino.moveDown())
    val newBoard = placeTetromino(frame.board, frame.tetromino, frame.tetromino.cellType)
    frame = frame.copy(board = newBoard)
    frame = frame.copy(tetromino = spawnTetromino())
    frame = frame.copy(board = deleteRows(frame.board))
  }

  def isGameOver: Boolean = frame.gameOver

  def getCellType(p : Point): CellType =
  {
    val relativePoint = Point(p.x-frame.tetromino.center.x, p.y - frame.tetromino.center.y)
    if(frame.tetromino.blocks.contains(relativePoint)) frame.tetromino.cellType
    else frame.board(p.y)(p.x) // Finally return board for all the tetrominos hat are fixed in place
  }
}

object TetrisLogic {

  val FramesPerSecond: Int = 5 // change this to speed up or slow down the game

  val DrawSizeFactor = 1.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller

  def makeEmptyBoard(gridDims : Dimensions): Seq[Seq[CellType]] = {
    val emptyLine = Seq.fill(gridDims.width)(Empty)
    Seq.fill(gridDims.height)(emptyLine)
  }


  // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultDims.width and DefaultDims.height


  val DefaultWidth: Int = 10
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  val DefaultDims : Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)


  def apply() = new TetrisLogic(new ScalaRandomGen(),
    DefaultDims,
    makeEmptyBoard(DefaultDims))

}

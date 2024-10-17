package tetris.logic

abstract class Tetromino(val cellType: CellType, val blocks: List[Point], val center: Point) {
  def rotateLeft(): Tetromino
  def rotateRight(): Tetromino

  def moveLeft(): Tetromino
  def moveRight(): Tetromino
  def moveUp(): Tetromino
  def moveDown(): Tetromino

  def absoluteBlocks(move: Point = Point(0, 0)): List[Point] = {
    blocks.map{b => b + center + move}
  }
}

case class CenterRotatingTetromino(override val cellType: CellType, override val center: Point, override val blocks: List[Point]) extends Tetromino(cellType, blocks, center) {
  override def rotateRight(): Tetromino = {
    new CenterRotatingTetromino(cellType, center, blocks.map { p =>
      Point(-p.y, p.x)
    })
  }

  override def rotateLeft(): Tetromino = {
    new CenterRotatingTetromino(cellType, center, blocks.map { p =>
      Point(p.y, -p.x)
    })
  }

  def moveLeft(): CenterRotatingTetromino = {
    copy(center = Point(center.x - 1, center.y))
  }

  def moveRight(): CenterRotatingTetromino = {
    copy(center = Point(center.x + 1, center.y))
  }

  def moveUp(): CenterRotatingTetromino = {
    copy(center = Point(center.x, center.y - 1))
  }

  def moveDown(): CenterRotatingTetromino = {
    copy(center = Point(center.x, center.y + 1))
  }
}

case class NonRotatingTetromino(override val cellType: CellType, override val center: Point, override val blocks: List[Point]) extends Tetromino(cellType, blocks, center) {
  override def rotateRight(): Tetromino = this
  override def rotateLeft(): Tetromino = this

  def moveLeft(): NonRotatingTetromino = {
    copy(center = Point(center.x - 1, center.y))
  }

  def moveRight(): NonRotatingTetromino = {
    copy(center = Point(center.x + 1, center.y))
  }

  def moveUp(): NonRotatingTetromino = {
    copy(center = Point(center.x, center.y - 1))
  }

  def moveDown(): NonRotatingTetromino = {
    copy(center = Point(center.x, center.y + 1))
  }
}

case class IRotatingTetromino(override val cellType: CellType, override val center: Point, override val blocks: List[Point]) extends Tetromino(cellType, blocks, center) {
  override def rotateRight(): Tetromino = {
    new IRotatingTetromino(cellType, center, blocks.map { p =>
      Point(-p.y + 1, p.x)
    })
  }

  override def rotateLeft(): Tetromino = {
    new IRotatingTetromino(cellType, center, blocks.map { p =>
      Point(p.y, -p.x + 1)
    })
  }

  def moveLeft(): IRotatingTetromino = {
    copy(center = Point(center.x - 1, center.y))
  }

  def moveRight(): IRotatingTetromino = {
    copy(center = Point(center.x + 1, center.y))
  }

  def moveUp(): IRotatingTetromino = {
    copy(center = Point(center.x, center.y - 1))
  }

  def moveDown(): IRotatingTetromino = {
    copy(center = Point(center.x, center.y + 1))
  }
}


object Tetromino {
  def J(centerX: Int): Tetromino = CenterRotatingTetromino(JCell, Point(centerX - 1, 1), List(Point(0,0), Point(-1,0), Point(-1,-1), Point(1,0)))
  def L(centerX: Int): Tetromino = CenterRotatingTetromino(LCell, Point(centerX - 1, 1), List(Point(0,0), Point(-1,0), Point(1,-1), Point(1,0)))
  def S(centerX: Int): Tetromino = CenterRotatingTetromino(SCell, Point(centerX - 1, 1), List(Point(0,0), Point(-1,0), Point(0,-1), Point(1,-1)))
  def T(centerX: Int): Tetromino = CenterRotatingTetromino(TCell, Point(centerX - 1, 1), List(Point(0,0), Point(-1, 0), Point(0,-1), Point(1,0)))
  def Z(centerX: Int): Tetromino = CenterRotatingTetromino(ZCell, Point(centerX - 1, 1), List(Point(0,0), Point(-1, -1), Point(0,-1), Point(1,0)))
  def O(centerX: Int): Tetromino = NonRotatingTetromino(OCell, Point(centerX - 1, 1), List(Point(0,0), Point(0,-1), Point(1,-1), Point(1,0)))
  def I(centerX: Int): Tetromino = IRotatingTetromino(ICell, Point(centerX - 1, 1), List(Point(0,0), Point(-1,0), Point(1,0), Point(2,0)))

}

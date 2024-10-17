package tetris.logic

case class Frame(tetromino: Tetromino, board: Seq[Seq[CellType]], gameOver: Boolean = false) {}

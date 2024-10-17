package tetris.logic

// you can alter this file!

case class Point(x : Int, y : Int) {
  def +(other: Point): Point = Point(this.x + other.x, this.y + other.y)
  def -(other: Point): Point = Point(this.x - other.x, this.y - other.y)
}

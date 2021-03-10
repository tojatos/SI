package pl.krzysztofruczkowski

object Direction extends Enumeration {
  type Direction = Value
  val Up, Down, Left, Right = Value
  def isVertical(direction: Direction) = List(Up, Down).contains(direction)
  def isHorizontal(direction: Direction) = List(Left, Right).contains(direction)
  def opposing(direction: Direction): Direction = {
    direction match {
      case Left => Right
      case Right => Left
      case Up => Down
      case Down => Up
    }
  }
}

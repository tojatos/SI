package pl.krzysztofruczkowski.plateproblem

import pl.krzysztofruczkowski.plateproblem.Direction.{Direction, Down, Left, Right, Up}

case class Segment(direction: Direction, length: Int) {
  def getNextPoint(lastPoint: Point): Point = {
    direction match {
      case Up => Point(lastPoint.x, lastPoint.y + length)
      case Down => Point(lastPoint.x, lastPoint.y - length)
      case Left => Point(lastPoint.x - length, lastPoint.y)
      case Right => Point(lastPoint.x + length, lastPoint.y)
    }
  }
}

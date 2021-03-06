import scala.language.postfixOps

object Direction extends Enumeration {
  type Direction = Value
  val Up, Down, Left, Right = Value
}

import Direction._
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
case class Path(segments: List[Segment])
case class PlateSolution(paths: List[Path])
object Util {
  def getPoints(startPoint: Point, path: Path): List[Point] = {
    path.segments.foldLeft(List(startPoint))((points, segments) => segments.getNextPoint(points.head) :: points)
  }
}

case class PlateProblem(width: Int, height: Int, pairs: Set[(Point, Point)])
{
  def isOutsideOfPlate(point: Point): Boolean = point.x < 0 || point.y < 0 || point.x >= width || point.y >= height

  def fitness(plateSolution: PlateSolution): Int = {
    val allSegments = plateSolution.paths flatMap (_.segments)
    val k2 = allSegments.map(_.length).sum
    val k3 = allSegments.length
//    pairs.map(_._1).zip(plateSolution.paths).
//    val k4 = allSegments.
    //TODO
    1000 - k2 - k3
  }
}
object PlateProblem {
  def deserialize(from: Iterator[String]): PlateProblem = {
    val wh = from.next().split(';') map (_.toInt)
    val (w, h) = (wh(0), wh(1))
    val set: Set[(Point, Point)] = from map (_.split(';') map (_.toInt)) map {x => (Point(x(0), x(1)), Point(x(2), x(3)))} toSet

    PlateProblem(w, h, set)
  }
}

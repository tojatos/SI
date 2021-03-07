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
  def getPointsBetween(startPoint: Point, endPoint: Point): Set[Point] = {
    if(startPoint.x == endPoint.x) {
      val range =
        if (startPoint.y <= endPoint.y)
          startPoint.y + 1 until endPoint.y
        else
          startPoint.y - 1 until endPoint.y by -1
      range.map(Point(startPoint.x, _)).toSet
    } else if (startPoint.y == endPoint.y) {
      val range =
        if (startPoint.x <= endPoint.x)
          startPoint.x + 1 until endPoint.x
        else
          startPoint.x -1 until endPoint.x by -1
      range.map(Point(_, startPoint.y)).toSet
    } else Set()
  }
  def getPoints(startPoint: Point, path: Path): List[Point] = {
    val turningPoints = path.segments.foldLeft(List(startPoint))((points, segment) => segment.getNextPoint(points.head) :: points)
    turningPoints.foldLeft((List[Point](startPoint), startPoint))((tuple, nextPoint) => {
      val acc = tuple._1
      val lastPoint = tuple._2
      return acc ::: getPointsBetween(lastPoint, nextPoint).toList ::: List(nextPoint)
    })._1
  }
}

case class PlateProblem(width: Int, height: Int, pairs: Set[(Point, Point)])
{
  def isOutsideOfPlate(point: Point): Boolean = point.x < 0 || point.y < 0 || point.x >= width || point.y >= height

  def countIntersections(plateSolution: PlateSolution): Int = {
    val allPoints = pairs.toList.map(_._1).zip(plateSolution.paths).flatMap((Util.getPoints _).tupled)
    allPoints.size - allPoints.toSet.size
  }

  def fitness(plateSolution: PlateSolution): Int = {
    val k1 = countIntersections(plateSolution)
    val allSegments = plateSolution.paths flatMap (_.segments)
    val k2 = allSegments.map(_.length).sum
    val k3 = allSegments.length
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

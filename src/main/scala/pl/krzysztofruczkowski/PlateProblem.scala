package pl.krzysztofruczkowski

import pl.krzysztofruczkowski.Direction.{Direction, Down, Left, Right, Up}

import scala.language.postfixOps

object Direction extends Enumeration {
  type Direction = Value
  val Up, Down, Left, Right = Value
}
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

case class PlateProblem(width: Int, height: Int, pairs: Set[(Point, Point)])
{
  def getTrivialSolution: PlateSolution =
    PlateSolution(pairs.toList.map((Util.getTrivialPath _).tupled))

  def isOutsideOfPlate(point: Point): Boolean =
    point.x < 0 || point.y < 0 || point.x >= width || point.y >= height

  def getAllPoints(plateSolution: PlateSolution): Seq[Point] =
    pairs.toList.map(_._1).zip(plateSolution.paths).flatMap((Util.getPoints _).tupled)

  def countIntersections(allPoints: Seq[Point]): Int = {
    allPoints.size - allPoints.toSet.size
  }

  def fitness(plateSolution: PlateSolution): Double = {
    val allPoints = getAllPoints(plateSolution)
    val k1 = countIntersections(allPoints)
    val allSegments = plateSolution.paths flatMap (_.segments)
    val k2 = allSegments.map(_.length).sum
    val k3 = allSegments.length
    val k5 = allPoints.count(isOutsideOfPlate)
    //TODO
    1000 - k1 * 5 - k2 * 1.5 - k3 - k5 * 15
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

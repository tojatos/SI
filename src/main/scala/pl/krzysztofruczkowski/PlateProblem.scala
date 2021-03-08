package pl.krzysztofruczkowski

import pl.krzysztofruczkowski.Direction.{Direction, Down, Left, Right, Up}

import scala.language.postfixOps

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

case class PlateProblem(width: Int, height: Int, pairs: List[(Point, Point)])
{
  def getTrivialSolution: PlateSolution =
    PlateSolution(pairs.map((Util.getTrivialPath _).tupled))

  def isOutsideOfPlate(point: Point): Boolean =
    point.x < 0 || point.y < 0 || point.x >= width || point.y >= height

  def getAllPoints(plateSolution: PlateSolution): Seq[Point] =
    pairs.map(_._1).zip(plateSolution.paths).flatMap((Util.getPoints _).tupled)

  def countIntersections(allPoints: Seq[Point]): Int = {
//    println(allPoints)
//    println(allPoints.toSet)
    allPoints.size - allPoints.toSet.size
  }

  def fitness(plateSolution: PlateSolution): Double = {
    val allPoints = getAllPoints(plateSolution)
    val k1 = countIntersections(allPoints)
    val allSegments = plateSolution.paths flatMap (_.segments)
    val k2 = allSegments.map(_.length).sum
    val k3 = allSegments.length
    val k5 = allPoints.count(isOutsideOfPlate)
//    println("k1: " + k1)
//    println("k2: " + k2)
//    println("k3: " + k3)
//    println("k5: " + k5)
    10000 - k1 * Const.K1_WEIGHT - k2 * Const.K2_WEIGHT - k3 * Const.K3_WEIGHT - k5 * Const.K5_WEIGHT
  }
}
object PlateProblem {
  def deserialize(from: Iterator[String]): PlateProblem = {
    val wh = from.next().split(';') map (_.toInt)
    val (w, h) = (wh(0), wh(1))
    val pairs: List[(Point, Point)] = from map (_.split(';') map (_.toInt)) map {x => (Point(x(0), x(1)), Point(x(2), x(3)))} toList

    PlateProblem(w, h, pairs)
  }
}

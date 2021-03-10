package pl.krzysztofruczkowski.plateproblem

import scala.language.postfixOps
import scala.util.Random

case class PlateProblem(width: Int, height: Int, pairs: List[(Point, Point)])
{
  def getTrivialSolution: PlateSolution =
    PlateSolution(pairs.map((Util.getTrivialPath _).tupled))

  def isOutsideOfPlate(point: Point): Boolean =
    point.x < 0 || point.y < 0 || point.x >= width || point.y >= height

  def getAllPoints(plateSolution: PlateSolution): Seq[Point] =
    pairs.map(_._1).zip(plateSolution.paths).flatMap((Util.getPoints _).tupled)

  def countIntersections(allPoints: Seq[Point]): Int = allPoints.size - allPoints.toSet.size

  def fitness(plateSolution: PlateSolution): Double = {
    val allPoints = getAllPoints(plateSolution)
    val k1 = countIntersections(allPoints)
    val allSegments = plateSolution.paths flatMap (_.segments)
    val k2 = allSegments.map(_.length).sum
    val k3 = allSegments.length
    val k5 = allPoints.count(isOutsideOfPlate)

    - k1 * Const.K1_WEIGHT - k2 * Const.K2_WEIGHT - k3 * Const.K3_WEIGHT - k5 * Const.K5_WEIGHT
  }

  def generateRandomSolution(random: Random = new Random()): PlateSolution = {
    var s = getTrivialSolution
    (1 to pairs.length * 30) foreach (_ => {
      var x = s.randomMutate(random)
      var k5 = getAllPoints(x).count(isOutsideOfPlate)
      while(k5 >= 10) {
        x = s.randomMutate(random)
        k5 = getAllPoints(x).count(isOutsideOfPlate)
      }
      s = x
    })
    println(s)
    s
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

package pl.krzysztofruczkowski.plateproblem

import pl.krzysztofruczkowski.plateproblem.Direction._

import Numeric.Implicits._
import scala.util.Random

object Util {
  def splitNumberInThree(number: Int)(implicit random: Random) : (Int, Int, Int) = {
    val numbers = (1 to 2).map(_ => random.between(0, number + 1)).sorted
    (numbers(0), numbers(1) - numbers(0), number - numbers(1))
  }

  def getTrivialPath(startPoint: Point, endPoint: Point): Path = {
    val xDiff = endPoint.x - startPoint.x
    val yDiff = endPoint.y - startPoint.y
    var segments: List[Segment] = List()
    if (xDiff != 0) {
      val direction = if (xDiff > 0) Right else Left
      segments = Segment(direction, xDiff.abs) :: segments
    }
    if (yDiff != 0) {
      val direction = if (yDiff > 0) Up else Down
      segments = Segment(direction, yDiff.abs) :: segments
    }
    Path(segments)
  }

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
    val turningPoints = path.segments.foldLeft(List(startPoint))((points, segment) => segment.getNextPoint(points.head) :: points).reverse
    turningPoints.foldLeft(List[Point]())((tuple, nextPoint) => {
      if(tuple.nonEmpty) {
        val lastPoint = tuple.last
        val pointsBetween = getPointsBetween(lastPoint, nextPoint).toList
        tuple ::: pointsBetween ::: List(nextPoint)
      } else {
        List(nextPoint)
      }
    })
  }

  def mean[T: Numeric] (xs: Iterable[T] ): Double = xs.sum.toDouble / xs.size

  def variance[T: Numeric] (xs: Iterable[T] ): Double = {
    val avg = mean (xs)

    xs.map (_.toDouble).map (a => math.pow (a - avg, 2) ).sum / xs.size
  }

  def stdDev[T: Numeric] (xs: Iterable[T] ): Double = math.sqrt (variance (xs) )
}

package pl.krzysztofruczkowski
import Direction._

object Util {
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
    val turningPoints = path.segments.foldLeft(List(startPoint))((points, segment) => segment.getNextPoint(points.head) :: points)
    turningPoints.foldLeft((List[Point](startPoint), startPoint))((tuple, nextPoint) => {
      val acc = tuple._1
      val lastPoint = tuple._2
      (acc ::: getPointsBetween(lastPoint, nextPoint).toList ::: List(nextPoint), nextPoint)
    })._1
  }
}

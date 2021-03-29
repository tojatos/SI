package pl.krzysztofruczkowski

case class MapColoringSolution(pointsToColor: Map[(Int, Int), Option[Int]])

object MapColoringSolution {
  def empty(points: List[(Int, Int)]) = MapColoringSolution(points.map(p => p -> None).toMap)
}

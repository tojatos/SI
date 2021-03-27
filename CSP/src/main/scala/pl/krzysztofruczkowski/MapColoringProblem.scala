package pl.krzysztofruczkowski

import scala.util.Random


case class MapColoringProblem(connections: Map[(Int, Int),List[(Int, Int)]]) {
  def isSolution(solution: MapColoringSolution): Boolean = {
    if(connections.size != solution.pointsToColor.size) return false

    for ((point, otherPoints) <- connections) {
      val firstColor = solution.pointsToColor(point)
      for (otherPoint <- otherPoints) {
        val secondColor = solution.pointsToColor(otherPoint)
        if(firstColor != secondColor) return false
      }
    }
    true
  }
}

//object MapColoringProblem {
//  def generate(pointCount: Int, mapDimensions: (Int,Int))(implicit random: Random): MapColoringProblem = {
//    val (maxX, maxY) = mapDimensions
//    val randomPoints: List[(Int, Int)] = (1 to pointCount).map(_ => (random.between(0, maxX), random.between(0, maxY))).toList
//    val pointsMap = scala.collection.mutable.Map[(Int, Int),List[(Int, Int)]]()
//    for (point <- randomPoints) {
//      if (pointsMap.contains(poi))
//    }
//
//      pointsMap += point
//
//
//    }
////    randomPoints.fold()
//
//
//}
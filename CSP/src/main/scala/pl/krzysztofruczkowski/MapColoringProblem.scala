package pl.krzysztofruczkowski

case class MapColoringProblem(connections: Map[(Int, Int),List[(Int, Int)]]) {
  def satisfiesRequirements(solution: MapColoringSolution): Boolean =
    solution.pointsToColor.size == connections.size &&
      solution.pointsToColor.forall(_._2.isDefined) &&
      satisfiesWeakRequirements(solution)

  def satisfiesWeakRequirements(solution: MapColoringSolution): Boolean = {
    if(connections.size != solution.pointsToColor.size) return false

    for ((point, otherPoints) <- connections) {
      val firstColor = solution.pointsToColor(point)
      if(firstColor.isDefined) {
        for (otherPoint <- otherPoints) {
          val secondColor = solution.pointsToColor(otherPoint)
          if(secondColor.isDefined) {
            if(firstColor == secondColor) return false
          }
        }
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
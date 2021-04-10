package pl.krzysztofruczkowski

import scala.util.Random

object MapColoringGenerator {
  def generate(pointsCount: Int, mapSize: Int)(implicit random: Random = new Random()): Map[Point,List[Point]] = {
    val randomPoints: List[(Int, Int)] = PointUtils.generateRandom(pointsCount, mapSize)
    //    val randomPoints: List[(Int, Int)] = (1 to pointCount).map(_ => (random.between(0, maxX), random.between(0, maxY))).toList.distinct
    val pointsMap = scala.collection.mutable.Map[(Int, Int),List[(Int, Int)]]()

    //TODO: find a better end condition
    for (_ <- 1 to 10000) {
      val p: List[(Int, Int)] = random.shuffle(randomPoints).take(2)
      val p1 = p(0)
      val p2 = p(1)
      val isAlreadyConnected = pointsMap.get(p1) match {
        case None => false
        case Some(x) => x.contains(p2)
      }
      if(!isAlreadyConnected) {
        val doesNotIntersectWithAny = pointsMap.forall(p => {
          val p3 = p._1
          p._2.forall(p4 => !PointUtils.intersectsOrCollinear(p1, p2, p3, p4))
        })
        if(doesNotIntersectWithAny) {
          if(pointsMap.contains(p1)) {
            pointsMap(p1) = p2 :: pointsMap(p1)
          } else {
            pointsMap.addOne(p1, List(p2))
          }

          if(pointsMap.contains(p2)) {
            pointsMap(p2) = p1 :: pointsMap(p2)
          } else {
            pointsMap.addOne(p2, List(p1))
          }
        }
      }
    }
    pointsMap.toMap
  }
}
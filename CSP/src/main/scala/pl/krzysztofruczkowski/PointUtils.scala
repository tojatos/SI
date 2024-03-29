package pl.krzysztofruczkowski

import scala.util.Random

object PointUtils {
  // generates pointsCount random distinct points in range (1, maxRange)
  def generateRandom(pointsCount: Int, maxRange: Int)(implicit random: Random = new Random()): List[(Int, Int)] = {
    val combinations = (1 to maxRange).combinations(2).map(x => (x(0), x(1)))
    random.shuffle(combinations).take(pointsCount).toList
  }

  def intersectsOrCollinear(x1: Int, y1: Int, x2: Int, y2: Int, x3: Int, y3: Int, x4: Int, y4: Int): Boolean = {
    // https: //stackoverflow.com/a/3838357/7136056
    // if (Math.max(x1, x2) < Math.min(x3, x4)) return false
    val points = List((x1, y1), (x2, y2), (x3, y3), (x4, y4))
    if(points.toSet.size < 4) return false // allow intersection at point

    val a1 =
      if(x1 - x2 == 0) 0
      else (y1 - y2) / (x1 - x2)
    val a2 =
      if(x3 - x4 == 0) 0
      else (y3 - y4) / (x3 - x4)

    // parallel
    if (a1 == a2) {
      val a3 = {
        if(x1 - x3 == 0) 0
        else (y1 - y3) / (x1 - x3)
      }

      // return true if collinear or false if just parallel
      return a1 == a3
    }

    val b1 = y1 - a1 * x1
    val b2 = y3 - a2 * x3
//    val ya = a1 * x1 + b1
//    val yb = a2 * xa + b2
//    a1 * xa + b1 = a2 * xa + b2
    val xa = (b2 - b1) / (a1 - a2)
    if((xa < Math.max(Math.min(x1, x2), Math.min(x3, x4))) ||
      (xa > Math.min(Math.max(x1, x2), Math.max(x3, x4))))
    false
    else true

  }

  def intersectsOrCollinear(p1: (Int, Int), p2: (Int, Int), p3: (Int, Int), p4: (Int, Int)): Boolean =
    intersectsOrCollinear(p1._1, p1._2, p2._1, p2._2, p3._1, p3._2, p4._1, p4._2)
}

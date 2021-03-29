package pl.krzysztofruczkowski

import scala.util.Random

object PointUtils {
  // generates pointsCount random distinct points in range (1, maxRange)
  def generateRandom(pointsCount: Int, maxRange: Int)(implicit random: Random = new Random()): List[(Int, Int)] = {
    val combinations = (1 to maxRange).combinations(2).map(x => (x(0), x(1)))
    random.shuffle(combinations).take(pointsCount).toList
  }

  def intersects(x1: Int, y1: Int, x2: Int, y2: Int, x3: Int, y3: Int, x4: Int, y4: Int): Boolean = {
    // https: //stackoverflow.com/a/3838357/7136056
    //    val i1 = (Math.min(x1, x2), Math.max(x1, x2))
    //    val i2 = (Math.min(x3, x4), Math.max(x3, x4))
    if (Math.max(x1, x2) < Math.min(x3, x4)) return false
    val a1 =
      if(x1 - x2 == 0) 0
      else (y1 - y2) / (x1 - x2)
    val a2 =
      if(x3 - x4 == 0) 0
      else (y3 - y4) / (x3 - x4)

    // parallel
    // TODO: check if collinear
    if (a1 == a2) return false

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
}

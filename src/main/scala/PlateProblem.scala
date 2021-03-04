case class PlateProblem(width: Int, height: Int, pairs: Set[(Point, Point)])
object PlateProblem {
  def deserialize(from: Iterator[String]): PlateProblem = {
    val wh = from.next().split(';') map (_.toInt)
    val (w, h) = (wh(0), wh(1))
    val set: Set[(Point, Point)] = from map (_.split(';') map (_.toInt)) map {x => (Point(x(0), x(1)), Point(x(2), x(3)))} toSet

    PlateProblem(w, h, set)
  }
}

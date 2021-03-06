import Direction.{Right, Up}
import org.scalatest.FunSuite

import scala.language.postfixOps

class PlateProblemTest extends FunSuite {
  val problem: PlateProblem = PlateProblem(
    6,6,
    Set(
      (Point(1,3),Point(5,3)),
      (Point(3,1),Point(3,3))
    )
  )
  val solution: PlateSolution = PlateSolution(
    List(
      Path(
        List(Segment(Right, 4))
      ),
      Path(
        List(Segment(Up, 2))
      )
    )
  )
  test("Util.getPointsBetween works") {
    val points = Util.getPointsBetween(Point(1, 3), Point(5, 3))
    assert(points === (1 to 5).map(x => Point(x, 3)).toSet)
    val points2 = Util.getPointsBetween(Point(3, 8), Point(3, 10))
    assert(points2 === (8 to 10).map(y => Point(3, y)).toSet)
    val points3 = Util.getPointsBetween(Point(3, 8), Point(5, 10))
    assert(points3 === Set.empty)
    val points4 = Util.getPointsBetween(Point(3, 3), Point(3, 0))
    assert(points4 === (0 to 3).map(y => Point(3, y)).toSet)
  }
  test("Util.getPoints works") {
    val points = Util.getPoints(Point(1, 3), Path(List(Segment(Right, 4))))
    assert(points === (1 to 5).map(x => Point(x, 3)).toList)
  }
  test("Intersections count works") {
    val intersectionCount = problem.countIntersections(solution)
    assert(intersectionCount === 1)
  }
}

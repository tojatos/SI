import org.scalatest.FunSuite
import pl.krzysztofruczkowski.plateproblem
import pl.krzysztofruczkowski.plateproblem.Direction.{Right, Up}
import pl.krzysztofruczkowski.plateproblem._

import scala.language.postfixOps

class PlateProblemTest extends FunSuite {
  val problem: PlateProblem = plateproblem.PlateProblem(
    6,6,
    List(
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
  test("pl.krzysztofruczkowski.Util.getPointsBetween works") {
    val points = Util.getPointsBetween(Point(1, 3), Point(5, 3))
    assert(points === (2 to 4).map(x => Point(x, 3)).toSet)
    val points2 = Util.getPointsBetween(Point(3, 8), Point(3, 11))
    assert(points2 === (9 to 10).map(y => Point(3, y)).toSet)
    val points3 = Util.getPointsBetween(Point(3, 8), Point(5, 11))
    assert(points3 === Set.empty)
    val points4 = Util.getPointsBetween(Point(3, 3), Point(3, 0))
    assert(points4 === (1 to 2).map(y => Point(3, y)).toSet)
  }
  test("pl.krzysztofruczkowski.Util.getPoints works") {
    val points = Util.getPoints(Point(1, 3), Path(List(Segment(Right, 4))))
    assert(points === (1 to 5).map(x => Point(x, 3)).toList)
  }
  test("Intersections count works") {
    val intersectionCount = problem.countIntersections(problem.getAllPoints(solution))
    assert(intersectionCount === 1)
  }
  test("Trivial solution works") {
    assert(problem.getTrivialSolution === solution)
  }
}

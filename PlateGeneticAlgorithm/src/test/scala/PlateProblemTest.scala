import org.scalatest.FunSuite
import pl.krzysztofruczkowski.plateproblem
import pl.krzysztofruczkowski.plateproblem.Direction.{Right, Up}
import pl.krzysztofruczkowski.plateproblem._

import scala.language.postfixOps
import plateproblem.Direction._

import scala.util.Random

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

  test("Path fixing works") {
    val correctPath = Path(List(Segment(Left, 2), Segment(Down, 5), Segment(Right, 3)))
    val testPath1 = Path(List(Segment(Left, 2), Segment(Left, 0), Segment(Down, 5), Segment(Right, 3)))
    val testPath2 = Path(List(Segment(Left, 1), Segment(Up, 0), Segment(Left, 1), Segment(Up, 7), Segment(Left, 0), Segment(Down, 12), Segment(Right, 3)))
    val testPath3 = Path(List(Segment(Left, 2), Segment(Left, 1), Segment(Right, 5), Segment(Right, 0), Segment(Left, 5), Segment(Right, 1), Segment(Down, 5), Segment(Right, 3)))
    assert(correctPath == correctPath.fixed())
    assert(correctPath == testPath1.fixed())
    assert(correctPath == testPath2.fixed())
    assert(correctPath == testPath3.fixed())
  }

  test("Mutation works") {
    val p1 = Path(List(Segment(Down, 1), Segment(Left, 3), Segment(Down, 3)))
    val s1 = Path(List(Segment(Down, 2), Segment(Left, 3), Segment(Down, 2)))
    assert(p1.mutate(1, forward = false, 1) == s1)

    val p2 = Path(List(Segment(Down, 4)))
    val s2a = Path(List(Segment(Right, 1), Segment(Down, 4), Segment(Left, 1)))
    val s2b = Path(List(Segment(Right, 3), Segment(Down, 4), Segment(Left, 3)))
    val s2c = Path(List(Segment(Left, 1), Segment(Down, 4), Segment(Right, 1)))
    assert(p2.mutate(0, forward = true, 1) == s2a)
    assert(p2.mutate(0, forward = true, 3) == s2b)
    assert(p2.mutate(0, forward = false, 1) == s2c)

    val p3 = Path(List(Segment(Right, 1), Segment(Down, 4), Segment(Left, 1)))
    val s3 = Path(List(Segment(Down, 4)))
    assert(p3.mutate(1, forward = false, 1) == s3)

  }

  test("Split in three works") {
    implicit val random: Random = new Random(123)
    for (n <- 1 to 10) {
      val splittedList = (1 to 100).map(_ => Util.splitNumberInThree(n))
//      println(n)
//      println(splittedList)
      assert(splittedList.forall(p => p._1 + p._2 + p._3 == n))
    }
  }

  test("Mutation b works") {
    val p1 = Path(List(Segment(Down, 1), Segment(Left, 3), Segment(Down, 3)))
    val s1 = Path(List(Segment(Down, 2), Segment(Left, 3), Segment(Down, 2)))
    assert(p1.mutate_b(1, (3, 0, 0), 0, forward = false, 1) == s1)
    assert(p1.mutate_b(1, (0, 3, 0), 1, forward = false, 1) == s1)
    assert(p1.mutate_b(1, (0, 0, 3), 2, forward = false, 1) == s1)

    val p2 = Path(List(Segment(Right, 1), Segment(Down, 4), Segment(Left, 1)))
    val s2 = Path(List(Segment(Down, 4)))
    assert(p2.mutate_b(1, (4,0,0), 0, forward = false, 1) == s2)

    val p3 = Path(List(Segment(Left, 2), Segment(Down, 5), Segment(Right, 3)))
    val s3a = Path(List(Segment(Left, 2), Segment(Down, 3), Segment(Right, 1), Segment(Down, 2), Segment(Right, 2)))
    val s3b = Path(List(Segment(Left, 2), Segment(Down, 2), Segment(Right, 1), Segment(Down, 1), Segment(Left, 1), Segment(Down, 2), Segment(Right, 3)))
    val s3c = Path(List(Segment(Left, 1), Segment(Down, 1), Segment(Left, 1), Segment(Down, 4), Segment(Right, 3)))
    val s3d = Path(List(Segment(Left, 1), Segment(Down, 2), Segment(Left, 1), Segment(Down, 3), Segment(Right, 3)))
    assert(p3.mutate_b(1, (0,3,2), 2, forward = true, 1) == s3a)
    assert(p3.mutate_b(1, (2,1,2), 1, forward = true, 1) == s3b)
    assert(p3.mutate_b(1, (0,1,4), 1, forward = true, 1) == s3c)
    assert(p3.mutate_b(1, (2,0,3), 0, forward = true, 1) == s3d)
  }
}

import Direction.{Right, Up}
import org.scalatest.FunSuite

class PlateProblemTest extends FunSuite {
  test("Plate") {
    val problem = PlateProblem(6,6,Set((Point(1,3),Point(5,3)), (Point(3,1),Point(3,3))))
    val solution = PlateSolution(List(Path(List(Segment(Right, 4))), Path(List(Segment(Up, 2)))))
    val fitness = problem.fitness(solution)
    assert(fitness === 992)
  }
}

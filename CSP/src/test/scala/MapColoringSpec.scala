import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import pl.krzysztofruczkowski.{BacktrackSolver, MapColoringCSP}

class MapColoringSpec extends AnyWordSpec with Matchers {

  val m1 =
    Map(
      (1, 1) -> List((2, 1), (2, 2)),
      (2, 1) -> List((1, 1), (2, 2)),
      (2, 2) -> List((1, 1), (2, 1)),
    )
  val m2 =
    Map(
      (1, 1) -> List((1, 2), (2, 1), (2, 2)),
      (1, 2) -> List((1, 1), (2, 2)),
      (2, 1) -> List((1, 1), (2, 2)),
      (2, 2) -> List((1, 1), (1, 2), (2, 1)),
    )
//  val emptySolution1 = MapColoringSolution.empty(problem1.connections.keys.toList)
  val problem1_2 = new MapColoringCSP(m1, 2)
  val problem1_3 = new MapColoringCSP(m1, 3)
  val problem1_4 = new MapColoringCSP(m1, 4)

  val problem2_2 = new MapColoringCSP(m2, 2)
  val problem2_3 = new MapColoringCSP(m2, 3)
  val problem2_4 = new MapColoringCSP(m2, 4)

  "MapColoringProblem" must {
    "accept correct solution" in {
      val solution = Vector(Some(1), Some(2), Some(3))
      problem1_3.satisfiesConstraints(solution) shouldEqual true
      problem1_4.satisfiesConstraints(solution) shouldEqual true
    }

    "reject incorrect solution" in {
      val incorrectSolution = Vector(Some(1), Some(2), Some(1))
      problem1_2.satisfiesConstraints(incorrectSolution) shouldEqual false
      problem1_3.satisfiesConstraints(incorrectSolution) shouldEqual false
      problem1_4.satisfiesConstraints(incorrectSolution) shouldEqual false
    }

    "accept weak incorrect solution" in {
      val incorrectSolution = Vector(Some(1), Some(2), None)
      problem1_2.satisfiesWeakConstraints(incorrectSolution) shouldEqual true
      problem1_3.satisfiesWeakConstraints(incorrectSolution) shouldEqual true
      problem1_4.satisfiesWeakConstraints(incorrectSolution) shouldEqual true
    }

    "reject empty solution" in {
      val emptySolution = Vector.fill(3)(None)
      problem1_2.satisfiesConstraints(emptySolution) shouldEqual false
      problem1_3.satisfiesConstraints(emptySolution) shouldEqual false
      problem1_4.satisfiesConstraints(emptySolution) shouldEqual false
    }

    "accept weak empty solution" in {
      val emptySolution = Vector.fill(3)(None)
      problem1_2.satisfiesWeakConstraints(emptySolution) shouldEqual true
      problem1_3.satisfiesWeakConstraints(emptySolution) shouldEqual true
      problem1_4.satisfiesWeakConstraints(emptySolution) shouldEqual true
    }
  }

  "BacktrackSolver" must {
    "return correct number of solutions" in {
      BacktrackSolver(problem1_2).solve().size shouldEqual 0
      BacktrackSolver(problem1_3).solve().size shouldEqual 6
      BacktrackSolver(problem1_4).solve().size shouldEqual 24

      BacktrackSolver(problem2_2).solve().size shouldEqual 0
      BacktrackSolver(problem2_3).solve().size shouldEqual 6
      BacktrackSolver(problem2_4).solve().size shouldEqual 48
    }
  }
}

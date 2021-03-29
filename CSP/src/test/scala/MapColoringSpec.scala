import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import pl.krzysztofruczkowski.{BacktrackMapColoringSolver, MapColoringProblem, MapColoringSolution}

class MapColoringSpec extends AnyWordSpec with Matchers {

  val problem1 = MapColoringProblem(
    Map(
      (1, 1) -> List((2, 1), (2, 2)),
      (2, 1) -> List((1, 1), (2, 2)),
      (2, 2) -> List((1, 1), (2, 1)),
    )
  )
  val problem2 = MapColoringProblem(
    Map(
      (1, 1) -> List((1, 2), (2, 1), (2, 2)),
      (1, 2) -> List((1, 1), (2, 2)),
      (2, 1) -> List((1, 1), (2, 2)),
      (2, 2) -> List((1, 1), (1, 2), (2, 1)),
    )
  )
  val emptySolution1 = MapColoringSolution.empty(problem1.connections.keys.toList)

  "MapColoringProblem" must {
    "accept correct solution" in {
      val solution = MapColoringSolution(
        Map(
          (1, 1) -> Some(1),
          (2, 1) -> Some(2),
          (2, 2) -> Some(3),
        )
      )
      problem1.satisfiesRequirements(solution) shouldEqual true
    }

    "reject incorrect solution" in {
      val incorrectSolution = MapColoringSolution(
        Map(
          (1, 1) -> Some(1),
          (2, 1) -> Some(2),
          (2, 2) -> Some(1),
        )
      )
      problem1.satisfiesRequirements(incorrectSolution) shouldEqual false
    }

    "accept weak incorrect solution" in {
      val incorrectSolution = MapColoringSolution(
        Map(
          (1, 1) -> Some(1),
          (2, 1) -> Some(2),
          (2, 2) -> None,
        )
      )
      problem1.satisfiesWeakRequirements(incorrectSolution) shouldEqual true
    }

    "reject empty solution" in {
      problem1.satisfiesRequirements(emptySolution1) shouldEqual false
    }

    "accept weak empty solution" in {
      problem1.satisfiesWeakRequirements(emptySolution1) shouldEqual true
    }
  }

  "BacktrackMapColoringSolver" must {
    "return correct number of solutions" in {
      val solver1 = BacktrackMapColoringSolver(problem1)
      val solver2 = BacktrackMapColoringSolver(problem2)
      solver1.solve(2).size shouldEqual 0
      solver1.solve(3).size shouldEqual 6
      solver1.solve(4).size shouldEqual 24

      solver2.solve(2).size shouldEqual 0
      solver2.solve(3).size shouldEqual 6
      solver2.solve(4).size shouldEqual 48
    }
  }
}

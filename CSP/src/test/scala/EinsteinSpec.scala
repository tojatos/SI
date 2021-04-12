import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import pl.krzysztofruczkowski.Color._
import pl.krzysztofruczkowski.Nationality._
import pl.krzysztofruczkowski.Drink._
import pl.krzysztofruczkowski.Smoke._
import pl.krzysztofruczkowski.Pet._
import pl.krzysztofruczkowski._

class EinsteinSpec extends AnyWordSpec with Matchers {
  val problem = new EinsteinCSP()
  val correctSolution = Vector(Some(Yellow), Some(Blue), Some(Red), Some(Ivory), Some(Green), Some(Norwegian), Some(Ukrainian), Some(Englishman), Some(Spaniard), Some(Japanese), Some(Water), Some(Tea), Some(Milk), Some(OrangeJuice), Some(Coffee), Some(Kools), Some(Chesterfield), Some(OldGold), Some(LuckyStrike), Some(Parliament), Some(Fox), Some(Horse), Some(Snails), Some(Dog), Some(Zebra))
  "EinsteinProblem" must {
    "accept correct solution" in {
      problem.satisfiesConstraints(correctSolution) shouldEqual true
    }

    "reject incorrect solution" in {
      val incorrectSolution = Vector(Some(Blue), Some(Red), Some(Yellow), Some(Ivory), Some(Green), Some(Norwegian), Some(Ukrainian), Some(Englishman), Some(Spaniard), Some(Japanese), Some(Water), Some(Tea), Some(Milk), Some(OrangeJuice), Some(Coffee), Some(Kools), Some(Chesterfield), Some(OldGold), Some(LuckyStrike), Some(Parliament), Some(Fox), Some(Horse), Some(Snails), Some(Dog), Some(Zebra))
      problem.satisfiesConstraints(incorrectSolution) shouldEqual false
    }

    "accept weak incorrect solution" in {
      val incorrectSolution = Vector(Some(Blue), Some(Red), Some(Yellow), Some(Ivory), Some(Green), None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)
      problem.satisfiesWeakConstraints(incorrectSolution) shouldEqual true
    }

    "reject weak incorrect solution" in {
      val incorrectSolution = Vector(Some(Blue), Some(Red), Some(Red), Some(Ivory), Some(Green), Some(Norwegian), Some(Ukrainian), Some(Englishman), Some(Spaniard), Some(Japanese), Some(Water), Some(Tea), Some(Milk), Some(OrangeJuice), Some(Coffee), Some(Kools), Some(Chesterfield), Some(OldGold), Some(LuckyStrike), Some(Parliament), Some(Fox), Some(Horse), Some(Snails), Some(Dog), Some(Zebra))
      problem.satisfiesWeakConstraints(incorrectSolution) shouldEqual false
    }

    "accept weak empty solution" in {
      problem.satisfiesWeakConstraints(problem.emptyInstance.variables) shouldEqual true
    }

    "reject empty solution" in {
      problem.satisfiesConstraints(problem.emptyInstance.variables) shouldEqual false
    }
  }

  "BacktrackSolver" must {
    "return corrrect solution" in {
      BacktrackSolver(problem).solve() shouldEqual List(correctSolution)
    }
  }

  "ForwardCheckingSolver" must {
    "return corrrect solution" in {
      ForwardCheckingSolver(problem).solve() shouldEqual List(correctSolution)
    }
  }
}

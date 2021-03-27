import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import pl.krzysztofruczkowski._

class EinsteinSpec extends AnyWordSpec with Matchers {
  "EinsteinProblem" must {
    "accept correct solution" in {
      val correctSolution = EinsteinSolution(List(
        House(Option(Color.Yellow), Option(Nationality.Norwegian), Option(Drink.Water), Option(Smoke.Kools), Option(Pet.Fox)),
        House(Option(Color.Blue), Option(Nationality.Ukrainian), Option(Drink.Tea), Option(Smoke.Chesterfield), Option(Pet.Horse)),
        House(Option(Color.Red), Option(Nationality.Englishman), Option(Drink.Milk), Option(Smoke.OldGold), Option(Pet.Snails)),
        House(Option(Color.Ivory), Option(Nationality.Spaniard), Option(Drink.OrangeJuice), Option(Smoke.LuckyStrike), Option(Pet.Dog)),
        House(Option(Color.Green), Option(Nationality.Japanese), Option(Drink.Coffee), Option(Smoke.Parliament), Option(Pet.Zebra)),
      ))
      EinsteinProblem.satisfiesRequirements(correctSolution) shouldEqual true
    }
    "reject incorrect solution" in {
      val incorrectSolution = EinsteinSolution(List(
        House(Option(Color.Blue), Option(Nationality.Norwegian), Option(Drink.Water), Option(Smoke.Kools), Option(Pet.Fox)),
        House(Option(Color.Red), Option(Nationality.Ukrainian), Option(Drink.Tea), Option(Smoke.Chesterfield), Option(Pet.Horse)),
        House(Option(Color.Yellow), Option(Nationality.Englishman), Option(Drink.Milk), Option(Smoke.OldGold), Option(Pet.Snails)),
        House(Option(Color.Ivory), Option(Nationality.Spaniard), Option(Drink.OrangeJuice), Option(Smoke.LuckyStrike), Option(Pet.Dog)),
        House(Option(Color.Green), Option(Nationality.Japanese), Option(Drink.Coffee), Option(Smoke.Parliament), Option(Pet.Zebra)),
      ))
      EinsteinProblem.satisfiesRequirements(incorrectSolution) shouldEqual false
    }

    "accept weak correct solution" in {
      val correctSolution = EinsteinSolution(List(
        House(Option(Color.Blue), Option(Nationality.Norwegian), Option(Drink.Water), Option(Smoke.Kools), Option(Pet.Fox)),
        House(Option(Color.Red), Option(Nationality.Ukrainian), Option(Drink.Tea), Option(Smoke.Chesterfield), Option(Pet.Horse)),
        House(Option(Color.Yellow), Option(Nationality.Englishman), Option(Drink.Milk), Option(Smoke.OldGold), Option(Pet.Snails)),
        House(Option(Color.Ivory), Option(Nationality.Spaniard), Option(Drink.OrangeJuice), Option(Smoke.LuckyStrike), Option(Pet.Dog)),
        House(Option(Color.Green), Option(Nationality.Japanese), Option(Drink.Coffee), Option(Smoke.Parliament), Option(Pet.Zebra)),
      ))
      EinsteinProblem.satisfiesWeakRequirements(correctSolution) shouldEqual true
    }

    "reject weak incorrect solution" in {
      val incorrectSolution = EinsteinSolution(List(
        House(Option(Color.Blue), Option(Nationality.Norwegian), Option(Drink.Water), Option(Smoke.Kools), Option(Pet.Fox)),
        House(Option(Color.Red), Option(Nationality.Ukrainian), Option(Drink.Tea), Option(Smoke.Chesterfield), Option(Pet.Horse)),
        House(Option(Color.Red), Option(Nationality.Englishman), Option(Drink.Milk), Option(Smoke.OldGold), Option(Pet.Snails)),
        House(Option(Color.Ivory), Option(Nationality.Spaniard), Option(Drink.OrangeJuice), Option(Smoke.LuckyStrike), Option(Pet.Dog)),
        House(Option(Color.Green), Option(Nationality.Japanese), Option(Drink.Coffee), Option(Smoke.Parliament), Option(Pet.Zebra)),
      ))
      EinsteinProblem.satisfiesWeakRequirements(incorrectSolution) shouldEqual false
    }
  }

}

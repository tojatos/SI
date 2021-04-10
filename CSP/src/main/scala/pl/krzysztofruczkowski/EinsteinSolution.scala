package pl.krzysztofruczkowski

import pl.krzysztofruczkowski.Color.Color
import pl.krzysztofruczkowski.Drink.Drink
import pl.krzysztofruczkowski.Nationality.Nationality
import pl.krzysztofruczkowski.Pet.Pet
import pl.krzysztofruczkowski.Smoke.Smoke

case class EinsteinSolution(houses: Seq[House])
case class House(color: Option[Color], nationality: Option[Nationality], drink: Option[Drink], smoke: Option[Smoke], pet: Option[Pet])
object EinsteinSolution {
  def empty(): EinsteinSolution = EinsteinSolution(Seq.fill(5)(House.empty()))
}
object House {
  def empty(): House = House(None, None, None, None, None)
}

object Color extends Enumeration {
  type Color = Value
  val Red, Ivory, Green, Yellow, Blue = Value
}

object Nationality extends Enumeration {
  type Nationality = Value
  val Englishman, Norwegian, Japanese, Spaniard, Ukrainian = Value
}

object Drink extends Enumeration {
  type Drink = Value
  val Milk, Tea, Coffee, Water, OrangeJuice = Value
}

object Smoke extends Enumeration {
  type Smoke = Value
  val Kools, OldGold, Parliament, Chesterfield, LuckyStrike = Value
}

object Pet extends Enumeration {
  type Pet = Value
  val Fox, Horse, Snails, Dog, Zebra = Value
}

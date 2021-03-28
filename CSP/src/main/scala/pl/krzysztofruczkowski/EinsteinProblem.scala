package pl.krzysztofruczkowski

object EinsteinProblem {
  val requirementList = List[EinsteinSolution => Boolean](
    e => e.houses.length == 5,
    e =>
      Color.values.forall(c => e.houses.exists(h => h.color.contains(c)))
        && Nationality.values.forall(c => e.houses.exists(h => h.nationality.contains(c)))
        && Drink.values.forall(c => e.houses.exists(h => h.drink.contains(c)))
        && Smoke.values.forall(c => e.houses.exists(h => h.smoke.contains(c)))
        && Pet.values.forall(c => e.houses.exists(h => h.pet.contains(c))),
    e => e.houses.find(_.color.contains(Color.Red)).exists(_.nationality.contains(Nationality.Englishman)),
    e => e.houses.find(_.nationality.contains(Nationality.Spaniard)).exists(_.pet.contains(Pet.Dog)),
    e => e.houses.find(_.drink.contains(Drink.Coffee)).exists(_.color.contains(Color.Green)),
    e => e.houses.find(_.nationality.contains(Nationality.Ukrainian)).exists(_.drink.contains(Drink.Tea)),
    e => e.houses.indexWhere(_.color.contains(Color.Green)) == e.houses.indexWhere(_.color.contains(Color.Ivory)) + 1,
    e => e.houses.find(_.smoke.contains(Smoke.OldGold)).exists(_.pet.contains(Pet.Snails)),
    e => e.houses.find(_.smoke.contains(Smoke.Kools)).exists(_.color.contains(Color.Yellow)),
    e => e.houses(2).drink.contains(Drink.Milk),
    e => e.houses.head.nationality.contains(Nationality.Norwegian),
    e => Math.abs(e.houses.indexWhere(_.smoke.contains(Smoke.Chesterfield)) - e.houses.indexWhere(_.pet.contains(Pet.Fox))) == 1,
    e => Math.abs(e.houses.indexWhere(_.smoke.contains(Smoke.Kools)) - e.houses.indexWhere(_.pet.contains(Pet.Horse))) == 1,
    e => e.houses.find(_.smoke.contains(Smoke.LuckyStrike)).exists(_.drink.contains(Drink.OrangeJuice)),
    e => e.houses.find(_.smoke.contains(Smoke.Parliament)).exists(_.nationality.contains(Nationality.Japanese)),
    e => Math.abs(e.houses.indexWhere(_.nationality.contains(Nationality.Norwegian)) - e.houses.indexWhere(_.color.contains(Color.Blue))) == 1,
  )

  val weakRequirementList = List[EinsteinSolution => Boolean](
    e => e.houses.length == 5,
    e => {
      def isDistinct[A](x: List[A]): Boolean = x.toSet.size == x.size

      val colors = e.houses.map(_.color).filter(_.isDefined).map(_.get)
      val nationalities = e.houses.map(_.nationality).filter(_.isDefined).map(_.get)
      val drinks = e.houses.map(_.drink).filter(_.isDefined).map(_.get)
      val smokes = e.houses.map(_.smoke).filter(_.isDefined).map(_.get)
      val pets = e.houses.map(_.pet).filter(_.isDefined).map(_.get)
      isDistinct(colors) && isDistinct(nationalities) && isDistinct(drinks) && isDistinct(smokes) && isDistinct(pets)
    },
    e => e.houses.filter(_.color.contains(Color.Red)).forall(x => x.nationality.isEmpty || x.nationality.contains(Nationality.Englishman)),
    e => e.houses.filter(_.nationality.contains(Nationality.Spaniard)).forall(x => x.pet.isEmpty || x.pet.contains(Pet.Dog)),
    e => e.houses.filter(_.drink.contains(Drink.Coffee)).forall(x => x.color.isEmpty || x.color.contains(Color.Green)),
    e => e.houses.filter(_.nationality.contains(Nationality.Ukrainian)).forall(x => x.drink.isEmpty || x.drink.contains(Drink.Tea)),
    e => {
      val i1 = e.houses.indexWhere(_.color.contains(Color.Green))
      val i2 = e.houses.indexWhere(_.color.contains(Color.Ivory))
      i1 == -1 || i2 == -1 || i1 == i2 + 1
    },
    e => e.houses.filter(_.smoke.contains(Smoke.OldGold)).forall(x => x.pet.isEmpty || x.pet.contains(Pet.Snails)),
    e => e.houses.filter(_.smoke.contains(Smoke.Kools)).forall(x => x.color.isEmpty || x.color.contains(Color.Yellow)),
    e => e.houses(2).drink.isEmpty || e.houses(2).drink.contains(Drink.Milk),
    e => e.houses.head.nationality.isEmpty || e.houses.head.nationality.contains(Nationality.Norwegian),
    e => {
      val i1 = e.houses.indexWhere(_.smoke.contains(Smoke.Chesterfield))
      val i2 = e.houses.indexWhere(_.pet.contains(Pet.Fox))
      i1 == -1 || i2 == -1 || Math.abs(i1 - i2) == 1
    },
    e => {
      val i1 = e.houses.indexWhere(_.smoke.contains(Smoke.Kools))
      val i2 = e.houses.indexWhere(_.pet.contains(Pet.Horse))
      i1 == -1 || i2 == -2 || Math.abs(i1 - i2) == 1
    },
    e => e.houses.filter(_.smoke.contains(Smoke.LuckyStrike)).forall(x => x.drink.isEmpty || x.drink.contains(Drink.OrangeJuice)),
    e => e.houses.find(_.smoke.contains(Smoke.Parliament)).forall(x => x.nationality.isEmpty || x.nationality.contains(Nationality.Japanese)),
    e => {
      val i1 = e.houses.indexWhere(_.nationality.contains(Nationality.Norwegian))
      val i2 = e.houses.indexWhere(_.color.contains(Color.Blue))
      i1 == -1 || i2 == -2 || Math.abs(i1 - i2) == 1
    },
  )
  def satisfiesRequirements(es: EinsteinSolution): Boolean = requirementList.forall(r => r(es))
  def satisfiesWeakRequirements(es: EinsteinSolution): Boolean = weakRequirementList.forall(r => r(es))

}

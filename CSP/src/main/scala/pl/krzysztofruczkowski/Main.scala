package pl.krzysztofruczkowski

object Main extends App {
  val emptySolution = EinsteinSolution.empty()
  var solutionList = List[EinsteinSolution]()
  var iterationSolution = emptySolution

  def iterateSolution(i: Int, currentSolution: EinsteinSolution): Unit = {
    if (i > 4) {
      if(EinsteinProblem.satisfiesRequirements(currentSolution)) {
        solutionList = currentSolution :: solutionList
      }
      return
    }
    Color.values.foreach { color =>
      val coloredHouse = currentSolution.houses(i).copy(color = Some(color))
      val coloredSolution = currentSolution.copy(houses = currentSolution.houses.updated(i, coloredHouse))
      if (EinsteinProblem.satisfiesWeakRequirements(coloredSolution)) {
        Nationality.values.foreach { nationality =>
          val nationalHouse = coloredSolution.houses(i).copy(nationality = Some(nationality))
          val nationalSolution = coloredSolution.copy(houses = coloredSolution.houses.updated(i, nationalHouse))
          if (EinsteinProblem.satisfiesWeakRequirements(nationalSolution)) {
            Drink.values.foreach { drink =>
              val drinkHouse = nationalSolution.houses(i).copy(drink = Some(drink))
              val drinkSolution = nationalSolution.copy(houses = nationalSolution.houses.updated(i, drinkHouse))
              if (EinsteinProblem.satisfiesWeakRequirements(drinkSolution)) {
                Smoke.values.foreach { smoke =>
                  val smokeHouse = drinkSolution.houses(i).copy(smoke = Some(smoke))
                  val smokeSolution = drinkSolution.copy(houses = drinkSolution.houses.updated(i, smokeHouse))
                  if (EinsteinProblem.satisfiesWeakRequirements(smokeSolution)) {
                    Pet.values.foreach { pet =>
                      val petHouse = smokeSolution.houses(i).copy(pet = Some(pet))
                      val petSolution = smokeSolution.copy(houses = smokeSolution.houses.updated(i, petHouse))
                      if (EinsteinProblem.satisfiesWeakRequirements(petSolution)) {
                        iterateSolution(i+1, petSolution)
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

  }
  println("Iteration begin")
  iterateSolution(0, emptySolution)
  println("Iteration finish")
  solutionList foreach println
}

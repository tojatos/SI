package pl.krzysztofruczkowski

import com.softwaremill.quicklens._

object BacktrackEinsteinSolver {
  def solve(): List[EinsteinSolution] = {
    val emptySolution = EinsteinSolution.empty()
    var solutionList = List[EinsteinSolution]()

    def iterateSolution(i: Int, currentSolution: EinsteinSolution): Unit = {
      if (i > 4) {
        if(EinsteinProblem.satisfiesRequirements(currentSolution)) {
          solutionList = currentSolution :: solutionList
        }
        return
      }
      Color.values.foreach { color =>
        val coloredSolution = currentSolution.modify(_.houses.at(i).color).setTo(Some(color))
        if (EinsteinProblem.satisfiesWeakRequirements(coloredSolution)) {
          Nationality.values.foreach { nationality =>
            val nationalSolution = coloredSolution.modify(_.houses.at(i).nationality).setTo(Some(nationality))
            if (EinsteinProblem.satisfiesWeakRequirements(nationalSolution)) {
              Drink.values.foreach { drink =>
                val drinkSolution = nationalSolution.modify(_.houses.at(i).drink).setTo(Some(drink))
                if (EinsteinProblem.satisfiesWeakRequirements(drinkSolution)) {
                  Smoke.values.foreach { smoke =>
                    val smokeSolution = drinkSolution.modify(_.houses.at(i).smoke).setTo(Some(smoke))
                    if (EinsteinProblem.satisfiesWeakRequirements(smokeSolution)) {
                      Pet.values.foreach { pet =>
                        val petSolution = smokeSolution.modify(_.houses.at(i).pet).setTo(Some(pet))
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
    iterateSolution(0, emptySolution)
    solutionList
  }
}

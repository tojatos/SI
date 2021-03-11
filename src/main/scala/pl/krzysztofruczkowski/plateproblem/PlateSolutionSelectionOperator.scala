package pl.krzysztofruczkowski.plateproblem

trait PlateSolutionSelectionOperator {
  def generateNewPopulation(population: List[PlateSolution]): List[PlateSolution]
}

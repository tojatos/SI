package pl.krzysztofruczkowski.plateproblem

trait PlateSolutionSelectionOperator {
  def select(population: List[ConcretePlateSolution]): ConcretePlateSolution
  def name: String
}

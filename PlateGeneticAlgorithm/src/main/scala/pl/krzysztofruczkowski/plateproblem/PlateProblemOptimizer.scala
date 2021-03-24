package pl.krzysztofruczkowski.plateproblem

trait PlateProblemOptimizer {
  def getBest(): ConcretePlateSolution
  def iterate(): Unit
}

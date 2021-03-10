package pl.krzysztofruczkowski.plateproblem

abstract class PlateProblemOptimizer(plateProblem: PlateProblem) {
  def getBest: PlateSolution
  def iterate(): Unit
}

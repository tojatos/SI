package pl.krzysztofruczkowski

abstract class PlateProblemOptimizer(plateProblem: PlateProblem) {
  def getBest: PlateSolution
  def iterate(): Unit
}

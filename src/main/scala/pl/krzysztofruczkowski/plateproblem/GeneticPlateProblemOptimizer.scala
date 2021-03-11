package pl.krzysztofruczkowski.plateproblem

import scala.util.Random

class GeneticPlateProblemOptimizer(plateProblem: PlateProblem, selectionOperator: PlateSolutionSelectionOperator, seed: Long = new Random().nextLong()) extends PlateProblemOptimizer {
  val random = new Random(seed)
  println(s"Used seed: $seed")

  var population: List[ConcretePlateSolution] = (1 to Const.GENERIC_OPTIMIZER_POPULATION_SIZE) map (_ => {
    val solution = plateProblem.generateRandomSolution(random)
    val fitness = plateProblem.fitness(solution)
    ConcretePlateSolution(solution, fitness)
  }) toList
  var best: ConcretePlateSolution = population.maxBy(x => x.fitness)

  println(s"Initial fitness: ${best.fitness}")
  override def getBest = best

  override def iterate(): Unit = {
    ???
  }
}
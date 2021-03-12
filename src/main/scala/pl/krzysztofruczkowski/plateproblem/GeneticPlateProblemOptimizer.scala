package pl.krzysztofruczkowski.plateproblem

import scala.util.Random

class GeneticPlateProblemOptimizer(plateProblem: PlateProblem
                                   , selectionOperator: PlateSolutionSelectionOperator
                                   , seed: Long = new Random().nextLong()) extends PlateProblemOptimizer {
  implicit val random: Random = new Random(seed)
  println(s"Used seed: $seed")

  var population: List[ConcretePlateSolution] = (1 to Const.GENERIC_OPTIMIZER_POPULATION_SIZE) map (_ => {
    val solution = plateProblem.generateRandomSolution(random)
    val fitness = plateProblem.fitness(solution)
    ConcretePlateSolution(solution, fitness)
  }) toList
  var best: ConcretePlateSolution = population.maxBy(x => x.fitness)

  override def getBest() = best
  println(s"Initial fitness: ${getBest().fitness}")

  override def iterate(): Unit = {
    val newPopulation = getBest() :: population.indices.drop(1).map(_ => {
      val p1 = selectionOperator.select(population)
      val p2 = selectionOperator.select(population)
      var o1 = if (random.between(0f, 1f) <= Const.GENERIC_OPTIMIZER_CROSS_PROBABILITY) {
        val crossedSolution = p1.plateSolution.cross(p2.plateSolution, random)
        val crossedFitness = plateProblem.fitness(crossedSolution)
        ConcretePlateSolution(crossedSolution, crossedFitness)
      } else p1
      if (random.between(0f, 1f) <= Const.GENERIC_OPTIMIZER_MUTATION_PROBABILITY) {
        val mutatedSolution = o1.plateSolution.randomMutate(random)
        val crossedFitness = plateProblem.fitness(mutatedSolution)

        o1 = ConcretePlateSolution(mutatedSolution, crossedFitness)
      }
      o1
    }).toList
    population = newPopulation

    val newBest = population.maxBy(x => x.fitness)
    if(newBest.fitness > best.fitness) {
      best = newBest
      println("!!!!!!!! " + best.fitness)
    }
  }
}
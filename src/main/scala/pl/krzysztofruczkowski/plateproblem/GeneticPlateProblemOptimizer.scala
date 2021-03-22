package pl.krzysztofruczkowski.plateproblem

import scala.util.Random
import scala.collection.parallel.CollectionConverters._

class GeneticPlateProblemOptimizer(plateProblem: PlateProblem,
                                   geneticPlateProblemParameters: GeneticPlateProblemParameters,
                                   )(implicit random: Random) extends PlateProblemOptimizer {

  val selectionOperator = geneticPlateProblemParameters.operator
  var population: List[ConcretePlateSolution] = (1 to Const.GENERIC_OPTIMIZER_POPULATION_SIZE).par.map (_ => {
    val solution = plateProblem.generateRandomSolution(random)
    val fitness = plateProblem.fitness(solution)
    ConcretePlateSolution(solution, fitness)
  }).toList
  var best: ConcretePlateSolution = population.maxBy(x => x.fitness)

  override def getBest() = best
  println(s"Initial fitness: ${getBest().fitness}")

  override def iterate(): Unit = {
    val newPopulation = getBest() :: population.indices.drop(1).par.map(_ => {
      val p1 = selectionOperator.select(population)
      var o1 = if (random.between(0f, 1f) <= geneticPlateProblemParameters.crossProbabilitiy) {
        val p2 = selectionOperator.select(population)
        val crossedSolution = p1.plateSolution.cross(p2.plateSolution, random)
        val crossedFitness = plateProblem.fitness(crossedSolution)
        ConcretePlateSolution(crossedSolution, crossedFitness)
      } else p1
      if (random.between(0f, 1f) <= geneticPlateProblemParameters.mutationProbabilitiy) {
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
      println("!!!!!!!! " + best.plateSolution)
    }
  }
}
package pl.krzysztofruczkowski.plateproblem

import scala.util.Random

class RandomMutationPlateProblemOptimizer(plateProblem: PlateProblem, seed: Long = new Random().nextLong()) extends PlateProblemOptimizer {
  val random = new Random(seed)
  println(s"Used seed: $seed")

  var best: ConcretePlateSolution = {
    val solution = plateProblem.generateRandomSolution(random)
    val fitness = plateProblem.fitness(solution)
    ConcretePlateSolution(solution, fitness)
  }
  var last: ConcretePlateSolution = best

  println(s"Initial fitness: ${best.fitness}")
  override def getBest() = best

  var iteration = 0
  override def iterate(): Unit = {
    iteration += 1
    if(iteration % Const.RANDOM_PLATE_PO_RESET_EVERY == 0) last = best //reset to best every x times
    val newSolution: PlateSolution = last.plateSolution.randomMutate(random)
    val newFitness = plateProblem.fitness(newSolution)
    last = ConcretePlateSolution(newSolution, newFitness)
    if (newFitness > best.fitness) {
      best = last
//      println("!!!!!!!! " + best.plateSolution)
//      println("!!!!!!!! " + best.fitness)
    }
  }
}
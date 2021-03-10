package pl.krzysztofruczkowski.plateproblem

import scala.util.Random

class RandomMutationPlateProblemOptimizer(plateProblem: PlateProblem, seed: Long = new Random().nextLong()) extends PlateProblemOptimizer(plateProblem) {
  val random = new Random(seed)
  println(s"Used seed: $seed")

  var best: PlateSolution = plateProblem.generateRandomSolution(random)
  var last: PlateSolution = best
  var bestFitness: Double = plateProblem.fitness(best)

  println(s"Initial fitness: $bestFitness")
  override def getBest = best

  var iteration = 0
  override def iterate(): Unit = {
    iteration += 1
    if(iteration % Const.RANDOM_PLATE_PO_RESET_EVERY == 0) last = best //reset to best every x times
    val newSolution: PlateSolution = last.randomMutate(random)
    last = newSolution
    val newFitness = plateProblem.fitness(newSolution)
    if (newFitness > bestFitness) {
      best = newSolution
      bestFitness = newFitness
      println("!!!!!!!! " + best)
      println("!!!!!!!! " + bestFitness)
    }
  }
}
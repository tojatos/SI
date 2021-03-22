package pl.krzysztofruczkowski.plateproblem

import scala.util.Random

class RandomPlateProblemOptimizer(plateProblem: PlateProblem, seed: Long = new Random().nextLong()) extends PlateProblemOptimizer {
  val random = new Random(seed)
  println(s"Used seed: $seed")

  var best: ConcretePlateSolution = {
    val solution = plateProblem.generateRandomSolution()
    val fitness = plateProblem.fitness(solution)
    ConcretePlateSolution(solution, fitness)
  }
  println(s"Initial fitness: ${best.fitness}")
  override def getBest() = best

  var iteration = 0
  override def iterate(): Unit = {
    iteration += 1
    val newSolution: PlateSolution = plateProblem.generateRandomSolution()
    val newFitness = plateProblem.fitness(newSolution)
    if (newFitness > best.fitness) {
      best = ConcretePlateSolution(newSolution, newFitness)
//      println("!!!!!!!! " + best.fitness)
    }
  }
}
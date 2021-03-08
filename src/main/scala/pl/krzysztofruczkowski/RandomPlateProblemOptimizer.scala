package pl.krzysztofruczkowski

class RandomPlateProblemOptimizer(plateProblem: PlateProblem) extends PlateProblemOptimizer(plateProblem) {

  var best: PlateSolution = plateProblem.getTrivialSolution
  var bestFitness: Double = plateProblem.fitness(best)

  def mutate(plateSolution: PlateSolution): Unit = {
    ???
//    plateSolution.paths.map(p => {
//      // TODO
//      val segments = p.segments
//
//    })

  }

  override def getBest = best

  override def iterate(): Unit = {
    val newSolution: PlateSolution = ???
    val newFitness = plateProblem.fitness(newSolution)
    if (newFitness > bestFitness) {
      best = newSolution
      bestFitness = newFitness
    }
  }
}

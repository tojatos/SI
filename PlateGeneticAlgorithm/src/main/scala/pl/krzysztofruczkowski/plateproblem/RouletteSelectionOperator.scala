package pl.krzysztofruczkowski.plateproblem

import scala.util.Random

class RouletteSelectionOperator(implicit random: Random) extends PlateSolutionSelectionOperator {
  def select(population: List[ConcretePlateSolution]): ConcretePlateSolution = {
    val reciprocalSum = population.map(-1 / _.fitness).sum
    val sumList = population.scanLeft(0d)((x, y) => x + ((-1/y.fitness) / reciprocalSum))
    val rand = random.between(0d, 1d)
    val index = sumList.indexWhere(x => x > rand) - 1
    population(index)
  }

  override def name = "Roulette"
}

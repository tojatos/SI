package pl.krzysztofruczkowski.plateproblem

import scala.util.Random

class TournamentSelectionOperator(n: Int)(implicit random: Random) extends PlateSolutionSelectionOperator {
  def select(population: List[ConcretePlateSolution]): ConcretePlateSolution =
    random.shuffle(population).take(n).maxBy(_.fitness)

  override def name = s"Tournament ${n}"
}

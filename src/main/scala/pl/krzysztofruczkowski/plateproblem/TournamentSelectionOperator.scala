package pl.krzysztofruczkowski.plateproblem

import scala.util.Random

class TournamentSelectionOperator(random: Random) extends PlateSolutionSelectionOperator {
  def select(population: List[ConcretePlateSolution]): ConcretePlateSolution =
    random.shuffle(population).take(Const.TOURNAMENT_SELECTION_N).maxBy(_.fitness)
}

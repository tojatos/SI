package pl.krzysztofruczkowski.plateproblem

import scala.util.Random

case class GeneticPlateProblemParameters(populationSize: Int = Const.GENERIC_OPTIMIZER_POPULATION_SIZE,
                                         crossProbabilitiy: Double = Const.GENERIC_OPTIMIZER_CROSS_PROBABILITY,
                                         mutationProbabilitiy: Double = Const.GENERIC_OPTIMIZER_MUTATION_PROBABILITY,
                                         operator: PlateSolutionSelectionOperator = new TournamentSelectionOperator(Const.TOURNAMENT_SELECTION_N)(new Random())
)

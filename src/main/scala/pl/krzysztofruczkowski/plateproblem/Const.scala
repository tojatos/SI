package pl.krzysztofruczkowski.plateproblem

import scala.util.Random

object Const {
  val K1_WEIGHT = 700
  val K2_WEIGHT = 3
  val K3_WEIGHT = 1
  val K5_WEIGHT = 1500

  val GENERIC_OPTIMIZER_POPULATION_SIZE = 1000
  val GENERIC_OPTIMIZER_CROSS_PROBABILITY = 0.7
  val GENERIC_OPTIMIZER_MUTATION_PROBABILITY = 0.04

  val TOURNAMENT_SELECTION_N = 3

  val RANDOM_PLATE_PO_RESET_EVERY = 200

  val PROBLEM_CONTROLLER_NUMBER_OF_ITERATIONS = 100
  val PROBLEM_CONTROLLER_REDRAW_EVERY = 200
  val OPERATOR = new RouletteSelectionOperator(new Random())
//  val OPERATOR = new TournamentSelectionOperator(new Random())

  val MUTATION_MULTIPLIER_MIN = 1
  val MUTATION_MULTIPLIER_MAX = 10
}

package pl.krzysztofruczkowski

import pl.krzysztofruczkowski.plateproblem.{Const, GeneticPlateProblemOptimizer, Loader, PlateProblem, TournamentSelectionOperator}

import scala.util.Random

object Main extends App {
  val filenames: Seq[String] = 0 to 3 map (x => s"zad$x.txt" )
  val problems: Seq[PlateProblem] = Loader.loadProblems(filenames)
  implicit val random: Random = new Random()
  val t1 = System.nanoTime()
  println(s"Generating random population (${Const.GENERIC_OPTIMIZER_POPULATION_SIZE})")
  val problem = problems(1)
  val po = new GeneticPlateProblemOptimizer(problem, new TournamentSelectionOperator(random))
  var duration = (System.nanoTime() - t1) / 1e9d
  println(s"Generated in $duration")
  while (!problem.isValid(po.getBest().plateSolution)) {
      for (_ <- 1 to 1000) {
        po.iterate()
      }
  }
  println(po.getBest())
}
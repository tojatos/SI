package pl.krzysztofruczkowski

import pl.krzysztofruczkowski.plateproblem.{Const, GeneticPlateProblemOptimizer, GeneticPlateProblemParameters, Loader, PlateProblem, TournamentSelectionOperator, Util}

import scala.util.Random

object Main extends App {
  val filenames: Seq[String] = 0 to 3 map (x => s"zad$x.txt" )
  val problems: Seq[PlateProblem] = Loader.loadProblems(filenames)
  val savePath = "E:\\SI"

  val randomSeed = new Random().nextLong()
  println(s"Used seed: $randomSeed")

  implicit val random: Random = new Random(randomSeed)
  val t1 = System.nanoTime()


  val problem = problems(1)
//  while (!problem.isValid(po.getBest().plateSolution)) {
//      for (_ <- 1 to 1000) {
//        po.iterate()
//      }
//  }
//  println(po.getBest())
  for (i <- 1 to 10) {
      val params = GeneticPlateProblemParameters()
      println(s"Generating random population (${Const.GENERIC_OPTIMIZER_POPULATION_SIZE})")
      val po = new GeneticPlateProblemOptimizer(problem, params)
      var duration = (System.nanoTime() - t1) / 1e9d
      println(s"Generated in $duration")
      for (j <- 1 to 200) {
        po.iterate()
      }
    println(s"Iteration $i")
    println(s"Best: ${po.best.fitness}")
    println(s"Avg: ${po.population.map(_.fitness).sum / po.population.length}")
    println(s"Worst: ${po.population.minBy(_.fitness).fitness}")
    println(s"Std dev: ${Util.stdDev(po.population.map(_.fitness))}")
  }
}
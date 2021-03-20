package pl.krzysztofruczkowski

import pl.krzysztofruczkowski.plateproblem._

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.reflect.io.File
import scala.util.Random

object Main extends App {
  val filenames: Seq[String] = 0 to 3 map (x => s"zad$x.txt" )
  val problems: Seq[PlateProblem] = Loader.loadProblems(filenames)
//  val savePath = "E:\\si_research_results"

  val randomSeed = new Random().nextLong()
  println(s"Used seed: $randomSeed")

  implicit val random: Random = new Random(randomSeed)
  val t1 = System.nanoTime()

  val researchIters = 1000
  val researchTimes = 5

  val seriousProblemsWithNames = problems.zip(filenames.map(_.split('.')(0))).drop(1)
  val testPopulations = List(10, 100, 500, 1000, 2000, 5000, 10000)
  val testPopulationParameters = testPopulations.map(x => GeneticPlateProblemParameters(x))

  def get_filename(problemString: String, p: GeneticPlateProblemParameters): String =
    "research_results\\" + List(problemString, p.populationSize, p.operator.name, p.crossProbabilitiy, p.mutationProbabilitiy).mkString("_") + ".txt"

  def researchParameters(problemWithName: (PlateProblem,String), p: GeneticPlateProblemParameters): Unit = {
    val bestFitnessList = (1 to researchTimes).par.map { _ =>
      lazy val po = new GeneticPlateProblemOptimizer(problemWithName._1, p)
      for (_ <- 1 to researchIters) {
        po.iterate()
      }
      po.best.fitness
    }.toList
    val resultString = List(bestFitnessList.max, bestFitnessList.min, Util.mean(bestFitnessList), Util.stdDev(bestFitnessList)).mkString(" ")
    val filename = get_filename(problemWithName._2, p)
    File(filename).writeAll(resultString)
    println(filename, resultString)
  }


//  researchParameters(seriousProblemsWithNames.head, testPopulationParameters(1))

  // no need to run in parallel as it takes 100% of CPU anyways
  seriousProblemsWithNames.foreach { problemWithName =>
    testPopulationParameters.foreach { p =>
      println(s"Beginning testing ${get_filename(problemWithName._2, p)}")
      researchParameters(problemWithName, p)
    }
  }
//  for (i <- 1 to 10) {
//      val params = GeneticPlateProblemParameters()
//      println(s"Generating random population (${Const.GENERIC_OPTIMIZER_POPULATION_SIZE})")
//      val po = new GeneticPlateProblemOptimizer(problem, params)
//      var duration = (System.nanoTime() - t1) / 1e9d
//      println(s"Generated in $duration")
//      for (j <- 1 to 200) {
//        po.iterate()
//      }
//    println(s"Iteration $i")
//    println(s"Best: ${po.best.fitness}")
//    println(s"Avg: ${po.population.map(_.fitness).sum / po.population.length}")
//    println(s"Worst: ${po.population.minBy(_.fitness).fitness}")
//    println(s"Std dev: ${Util.stdDev(po.population.map(_.fitness))}")
//  }
}
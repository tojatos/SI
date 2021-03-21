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

  val seriousProblemsWithNames = problems.zip(filenames.map(_.split('.')(0))).drop(1)
  val testPopulations = List(10, 100, 500, 1000, 2000, 5000, 10000)
  val testPopulationParameters = testPopulations.map(x => GeneticPlateProblemParameters(x))

  val testTournamentSizes = List(1, 2, 3, 5, 10, 20, 100)
  val testTournamentParameters = testTournamentSizes.map(x => GeneticPlateProblemParameters(operator = new TournamentSelectionOperator(x)))

  val testOperators = List(new TournamentSelectionOperator(Const.TOURNAMENT_SELECTION_N), new RouletteSelectionOperator())
  val testOperatorsParameters = testOperators.map(x => GeneticPlateProblemParameters(operator = x))

  def get_filename(problemString: String, p: GeneticPlateProblemParameters): String =
    "research_results\\" + List(problemString, p.populationSize, p.operator.name, p.crossProbabilitiy, p.mutationProbabilitiy).mkString("_") + ".txt"

  def researchParameters(problemWithName: (PlateProblem,String), p: GeneticPlateProblemParameters): Unit = {
    val bestFitnessList = (1 to Const.RESEARCH_TIMES).par.map { _ =>
      lazy val po = new GeneticPlateProblemOptimizer(problemWithName._1, p)
      for (_ <- 1 to Const.RESEARCH_ITERATIONS) {
        po.iterate()
      }
      po.best.fitness
    }.toList
    val resultString = List(bestFitnessList.max, bestFitnessList.min, Util.mean(bestFitnessList), Util.stdDev(bestFitnessList)).mkString(" ")
    val filename = get_filename(problemWithName._2, p)
    File(filename).writeAll(resultString)
    println(filename, resultString)
  }

  def researchGenerations() {
    seriousProblemsWithNames.foreach { problemWithName =>
//       val muchData = (1 to 10).map({ x =>
          val params = GeneticPlateProblemParameters()
          val po = new GeneticPlateProblemOptimizer(problemWithName._1, params)
          val data = (1 to 2000).map { _ =>
            po.iterate()
            val best = po.best.fitness
            val worst = po.population.minBy(_.fitness).fitness
            val avg = po.population.map(_.fitness).sum / po.population.length
            val std = Util.stdDev(po.population.map(_.fitness))
            (best, worst, avg, std)
          }
//        data
//      })
//      (problemWithName._2, data)
      val filename = s"research_results\\${problemWithName._2}.txt"
      val stringData = data.mkString("\n")
      File(filename).writeAll(stringData)
      println(filename, stringData)
//      muchData.flatMap(_.zipWithIndex).groupBy { case (num, idx) => idx }
//        .map { case (k, v) => v.map(_._1).sum }.toList
    }
  }


  def research(parameters: List[GeneticPlateProblemParameters]) {
    // no need to run in parallel as it takes 100% of CPU anyways
    seriousProblemsWithNames.foreach { problemWithName =>
      parameters.foreach { p =>
        println(s"Beginning testing ${get_filename(problemWithName._2, p)}")
        researchParameters(problemWithName, p)
      }
    }
  }
//  researchGenerations()
  research(testOperatorsParameters)
}
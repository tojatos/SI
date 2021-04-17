package pl.krzysztofruczkowski

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Main extends App {
  def measure(solver: Solver): Unit = {
    val t1 = System.nanoTime
    def getTimeElapsed: Double = {
      val time = (System.nanoTime - t1) / 1e9d
      BigDecimal(time).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    }
    var firstTimeFound = false
    val f = Future {
      while (!solver.solved || !firstTimeFound) {
        if(!firstTimeFound && solver.firstSolutionFound) {
          firstTimeFound = true
          println("--- First solution found -----------------------")
          println(solver.firstSolutionVisitedNodes)
          println(getTimeElapsed)
          println("------------------------------------------------")
        }
        Thread.sleep(10)
      }
    }
    solver.solve()

    Await.ready(f, Duration.Inf)
    println("--- All solutions found ------------------------")
    println(solver.visitedNodes)
    println(getTimeElapsed)
    println("------------------------------------------------")

  }

//  def measureBacktrack(problem: CSP): Unit = {
//    val solver = BacktrackSolver(problem)
//    measure(solver)
//  }
//
//  def measureForwardChecking(problem: CSP): Unit = {
//    val solver = ForwardCheckingSolver(problem)
//    measure(solver)
//  }
//
//  def measureMapColoringForwardChecking(problem: MapColoringCSP): Unit = {
//    val solver = MapColoringForwardCheckingSolver(problem)
//    measure(solver)
//  }

  println("Generating problems...")
  val einsteinProblem = new EinsteinCSP()

  val availableColors = 4
  val nList = List(2, 3, 4, 6, 8, 9, 10, 12, 13, 14)
  val mapColoringMaps = nList.map(n => {
    var m = MapColoringGenerator.generate(n, 100)
    while (m.size != n) {
      m = MapColoringGenerator.generate(n, 100)
    }
    m
  })
//  println(mapColoringMaps.map(_.size))
  val mapColoringProblems = mapColoringMaps.map(m => new MapColoringCSP(m, availableColors))
  println("Problems generated.")

  for (problem <- mapColoringProblems) {
    println(problem.points.size)
    measure(MapColoringForwardCheckingSolver(problem))
  }

  for (problem <- mapColoringProblems) {
    println(problem.points.size)
    measure(BacktrackSolver(problem))
  }

//  measure(ForwardCheckingSolver(einsteinProblem))
//  measure(BacktrackSolver(einsteinProblem))

//  println(solver.solve())
//
//  val m1 =
//    Map(
//      (1, 1) -> List((2, 1), (2, 2)),
//      (2, 1) -> List((1, 1), (2, 2)),
//      (2, 2) -> List((1, 1), (2, 1)),
//    )
//  val mapColoringProblem = new MapColoringCSP(m1, 3)
//  println(BacktrackSolver(mapColoringProblem).solve().size)
//  val m2 = MapColoringGenerator.generate(10, 10)
//  println(m2)
//  println(m2.size)
//  val mapColoringProblem2_3 = new MapColoringCSP(m2, 3)
//  val mapColoringProblem2_4 = new MapColoringCSP(m2, 4)
//  val results1 = BacktrackSolver(mapColoringProblem2_3).solve()
//  val results2 = BacktrackSolver(mapColoringProblem2_4).solve()
//  println(results1.head)
//  println(results2.head)
}

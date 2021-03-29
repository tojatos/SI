package pl.krzysztofruczkowski

object Main extends App {
//  val einsteinSolutions = BacktrackEinsteinSolver.solve()
//  einsteinSolutions foreach println

  val mapColoringProblem = MapColoringProblem.generate(4, 10)
  println(mapColoringProblem)
  val results1 = BacktrackMapColoringSolver(mapColoringProblem).solve(3)
  val results2 = BacktrackMapColoringSolver(mapColoringProblem).solve(4)
  println(results1.head)
  println(results2.head)
}

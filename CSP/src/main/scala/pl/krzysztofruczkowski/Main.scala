package pl.krzysztofruczkowski

import pl.krzysztofruczkowski.Color.Color

object Main extends App {
  val problem = new EinsteinCSP()
  val solver = BacktrackSolver(problem)
  println(solver.solve())

//  val mapColoringProblem = MapColoringProblem.generate(4, 10)
//  println(mapColoringProblem)
//  val results1 = BacktrackMapColoringSolver(mapColoringProblem).solve(3)
//  val results2 = BacktrackMapColoringSolver(mapColoringProblem).solve(4)
//  println(results1.head)
//  println(results2.head)
}

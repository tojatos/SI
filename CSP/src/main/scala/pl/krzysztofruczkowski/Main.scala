package pl.krzysztofruczkowski

object Main extends App {
  val problem = new EinsteinCSP()
  val solver = BacktrackSolver(problem)
  println(solver.solve())

  val m1 =
    Map(
      (1, 1) -> List((2, 1), (2, 2)),
      (2, 1) -> List((1, 1), (2, 2)),
      (2, 2) -> List((1, 1), (2, 1)),
    )
  val mapColoringProblem = new MapColoringCSP(m1, 3)
  println(BacktrackSolver(mapColoringProblem).solve().size)
  val m2 = MapColoringGenerator.generate(4, 10)
  println(m2)
  val mapColoringProblem2_3 = new MapColoringCSP(m2, 3)
  val mapColoringProblem2_4 = new MapColoringCSP(m2, 4)
  val results1 = BacktrackSolver(mapColoringProblem2_3).solve()
  val results2 = BacktrackSolver(mapColoringProblem2_4).solve()
  println(results1.head)
  println(results2.head)
}

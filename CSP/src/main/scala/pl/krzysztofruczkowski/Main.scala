package pl.krzysztofruczkowski

object Main extends App {
  val einsteinSolutions = BacktrackEinsteinSolver.solve()
  einsteinSolutions foreach println
}

package pl.krzysztofruczkowski

abstract class Solver() {
  var visitedNodes: Int
  var firstSolutionFound: Boolean
  var firstSolutionVisitedNodes: Int
  var solved: Boolean
  def solve(): List[Seq[Variable]]

}

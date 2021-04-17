package pl.krzysztofruczkowski

import com.softwaremill.quicklens._

//TODO: add lock to solution list and run in parallel
case class BacktrackSolver(problem: CSP, variableSelector: VariableSelector = DefaultVariableSelector()) extends Solver() {
  override var visitedNodes = 0
  override var firstSolutionFound = false
  override var firstSolutionVisitedNodes = 0
  override var solved = false

  def solve(): List[Seq[Variable]] = {
    val emptyInstance = problem.emptyInstance
    var solutionList = List[Seq[Variable]]()

    def iterateSolution(currentSolution: CSPInstance): Unit = {
      if (currentSolution.variables.forall(_.isDefined)) {
        if (problem.satisfiesConstraints(currentSolution.variables)) {
          firstSolutionFound = true
          firstSolutionVisitedNodes = visitedNodes
          solutionList = currentSolution.variables :: solutionList
          return
        }
      }
      val i = variableSelector.selectIndex(currentSolution)
      currentSolution.domains(i).foreach(d => {
        val newSolution = currentSolution.modify(_.variables.at(i)).setTo(Some(d))
        visitedNodes += 1
        if (problem.satisfiesWeakConstraints(newSolution.variables)) {
          iterateSolution(newSolution)
        }
      })
    }
    visitedNodes = 0
    firstSolutionFound = false
    firstSolutionVisitedNodes = 0
    solved = false
    iterateSolution(emptyInstance)
    solved = true
    solutionList
  }
}

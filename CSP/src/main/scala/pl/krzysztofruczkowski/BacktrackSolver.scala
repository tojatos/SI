package pl.krzysztofruczkowski

import com.softwaremill.quicklens._

//TODO: add lock to solution list and run in parallel
case class BacktrackSolver(problem: CSP) {
  def solve(): List[Seq[Variable]] = {
    val emptyInstance = problem.emptyInstance
    var solutionList = List[Seq[Variable]]()

    def iterateSolution(i: Int, currentSolution: CSPInstance): Unit = {
      if (i >= emptyInstance.variables.length) {
        if (problem.satisfiesConstraints(currentSolution.variables)) {
          solutionList = currentSolution.variables :: solutionList
          return
        }
      }
      currentSolution.domains(i).foreach(d => {
        val newSolution = currentSolution.modify(_.variables.at(i)).setTo(Some(d))
        if (problem.satisfiesWeakConstraints(newSolution.variables)) {
          iterateSolution(i + 1, newSolution)
        }
      })
    }
    iterateSolution(0, emptyInstance)
    solutionList
  }
}

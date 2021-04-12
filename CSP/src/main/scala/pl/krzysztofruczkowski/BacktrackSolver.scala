package pl.krzysztofruczkowski

import com.softwaremill.quicklens._

//TODO: add lock to solution list and run in parallel
case class BacktrackSolver(problem: CSP, variableSelector: VariableSelector = DefaultVariableSelector()) {

  def solve(): List[Seq[Variable]] = {
    val emptyInstance = problem.emptyInstance
    var solutionList = List[Seq[Variable]]()

    def iterateSolution(currentSolution: CSPInstance): Unit = {
      if (currentSolution.variables.forall(_.isDefined)) {
        if (problem.satisfiesConstraints(currentSolution.variables)) {
          solutionList = currentSolution.variables :: solutionList
          return
        }
      }
      val i = variableSelector.selectIndex(currentSolution)
      currentSolution.domains(i).foreach(d => {
        val newSolution = currentSolution.modify(_.variables.at(i)).setTo(Some(d))
        if (problem.satisfiesWeakConstraints(newSolution.variables)) {
          iterateSolution(newSolution)
        }
      })
    }
    iterateSolution(emptyInstance)
    solutionList
  }
}

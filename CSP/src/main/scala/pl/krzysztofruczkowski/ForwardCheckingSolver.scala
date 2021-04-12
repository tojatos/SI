package pl.krzysztofruczkowski

import com.softwaremill.quicklens._

//TODO: add lock to solution list and run in parallel
case class ForwardCheckingSolver(problem: CSP, variableSelector: VariableSelector = MinDomainVariableSelector()) {
  def unassignedVariablesHaveLegalValues(i: CSPInstance): Boolean = {
    val available = i.variables zip i.domains filter(_._1.isEmpty)
    available.forall(_._2.nonEmpty)
  }

  def selectValue(currentSolution: CSPInstance, i: Int) = {
    currentSolution.domains(i).head
  }

  def getNewSolution(currentSolution: CSPInstance, i: Int, newValue: Any) = {
    val instanceWithUpdatedValue = currentSolution.modify(_.variables.at(i)).setTo(Some(newValue))
    val zipped = (instanceWithUpdatedValue.variables zip instanceWithUpdatedValue.domains).zipWithIndex
    val newDomains = zipped.map(x => {
      val (variable, domain) = x._1
      val index = x._2
      if(variable.isDefined) domain //do not change domain if defined
      else domain.filter(value => {
        val newVars = instanceWithUpdatedValue.variables.modify(_.at(index)).setTo(Some(value))
        problem.satisfiesWeakConstraints(newVars)
      })
    })
      val instanceWithPrunedDomain = instanceWithUpdatedValue.modify(_.domains).setTo(newDomains)
      instanceWithPrunedDomain
  }

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

      if(currentSolution.domains(i).isEmpty) return

      val newValue = selectValue(currentSolution, i)
      val newSolution = getNewSolution(currentSolution, i, newValue)
      if (unassignedVariablesHaveLegalValues(newSolution) && problem.satisfiesWeakConstraints(newSolution.variables)) {
        iterateSolution(newSolution)
      }
      // remove this value from domain
      val oldDomain = currentSolution.domains(i)
      val newDomain = oldDomain.filter(_ != newValue)
      val solutionWithUpdatedDomain = currentSolution.modify(_.domains.at(i)).setTo(newDomain)
      iterateSolution(solutionWithUpdatedDomain)
    }
    iterateSolution(emptyInstance)
    solutionList
  }
}

package pl.krzysztofruczkowski

import com.softwaremill.quicklens._


abstract class ValueSelector() {
  def selectValue(problem: MapColoringCSP, currentSolution: CSPInstance, i: Int): Any
}
case class DefaultValueSelector() extends ValueSelector() {
  override def selectValue(problem: MapColoringCSP, currentSolution: CSPInstance, i: Int) = {
    currentSolution.domains(i).head
  }
}

case class MinConstraintValueSelector() extends ValueSelector() {
  override def selectValue(problem: MapColoringCSP, currentSolution: CSPInstance, i: Int) = {
    val pointsConnectedToUpdatedValue = problem.connections(problem.points(i))
    val zipped = currentSolution.variables zip currentSolution.domains zip problem.points
    currentSolution.domains(i).minBy(value => {
      val otherDomains = pointsConnectedToUpdatedValue.map(p => zipped.find(_._2 == p).get).map(_._1._2)
      otherDomains.count(v => v.contains(value))
    })
  }
}

case class MaxConstraintValueSelector() extends ValueSelector() {
  override def selectValue(problem: MapColoringCSP, currentSolution: CSPInstance, i: Int) = {
    val pointsConnectedToUpdatedValue = problem.connections(problem.points(i))
    val zipped = currentSolution.variables zip currentSolution.domains zip problem.points
    currentSolution.domains(i).maxBy(value => {
      val otherDomains = pointsConnectedToUpdatedValue.map(p => zipped.find(_._2 == p).get).map(_._1._2)
      otherDomains.count(v => v.contains(value))
    })
  }
}

//TODO: add lock to solution list and run in parallel
case class MapColoringForwardCheckingSolver(problem: MapColoringCSP,
                                            variableSelector: VariableSelector = MinDomainVariableSelector(),
                                            valueSelector: ValueSelector = DefaultValueSelector()) extends Solver() {
  override var visitedNodes = 0
  override var firstSolutionFound = false
  override var solved = false
  override var firstSolutionVisitedNodes = 0


  def unassignedVariablesHaveLegalValues(i: CSPInstance): Boolean = {
    val available = i.variables zip i.domains filter(_._1.isEmpty)
    available.forall(_._2.nonEmpty)
  }

  def getNewSolution(currentSolution: CSPInstance, i: Int, newValue: Any) = {
    val instanceWithUpdatedValue = currentSolution.modify(_.variables.at(i)).setTo(Some(newValue))
    val pointsConnectedToUpdatedValue = problem.connections(problem.points(i))

    val zipped = instanceWithUpdatedValue.variables zip instanceWithUpdatedValue.domains zip problem.points
    val newDomains = zipped.map(x => {
      val ((variable, domain), point) = x
      if(variable.isDefined || !pointsConnectedToUpdatedValue.contains(point)) domain
      else domain.filter(_ != newValue)
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
          firstSolutionFound = true
          firstSolutionVisitedNodes = visitedNodes
          solutionList = currentSolution.variables :: solutionList
          return
        }
      }
      val i = variableSelector.selectIndex(currentSolution)

      if(currentSolution.domains(i).isEmpty) return

      val newValue = valueSelector.selectValue(problem, currentSolution, i)
      val newSolution = getNewSolution(currentSolution, i, newValue)
      visitedNodes += 1
      if (unassignedVariablesHaveLegalValues(newSolution) && problem.satisfiesWeakConstraints(newSolution.variables)) {
        iterateSolution(newSolution)
      }
      // remove this value from domain
      val oldDomain = currentSolution.domains(i)
      val newDomain = oldDomain.filter(_ != newValue)
      val solutionWithUpdatedDomain = currentSolution.modify(_.domains.at(i)).setTo(newDomain)
      iterateSolution(solutionWithUpdatedDomain)
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

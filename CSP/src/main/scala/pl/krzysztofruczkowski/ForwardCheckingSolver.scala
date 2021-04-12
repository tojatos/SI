package pl.krzysztofruczkowski

import com.softwaremill.quicklens._

import scala.annotation.tailrec

//TODO: add lock to solution list and run in parallel
//case class ForwardCheckingSolver(problem: CSP) {
//
//  def selectIndex(currentSolution: CSPInstance): Int = {
//    val available = (currentSolution.variables zip currentSolution.domains).zipWithIndex.filter(_._1._1.isEmpty)
//    available.head._2
//  }
//
//  def unassignedVariablesHaveLegalValues(i: CSPInstance): Boolean = {
//    val available = i.variables zip i.domains filter(_._1.isEmpty)
//    available.forall(_._2.nonEmpty)
//  }
//
//  def selectValue(currentSolution: CSPInstance, i: Int) = {
//    if(currentSolution.domains(i).isEmpty) None
//    else Some(currentSolution.domains(i).head) //TODO: use heuristics
//  }
//
//  def getNewSolution(currentSolution: CSPInstance, i: Int, newValue: Any) = {
//    val instanceWithUpdatedValue = currentSolution.modify(_.variables.at(i)).setTo(Some(newValue))
////    val oldDomain = instanceWithUpdatedValue.domains(i)
////    val newDomain = oldDomain.modify(_.at(i)).setTo(oldDomain.filterNot(newValue))
//    //TODO: forward check and adjust other domains
////    val instanceWithPrunedDomain = instanceWithUpdatedValue.modify(_.domains.at(i)).setTo(newDomain)
////    instanceWithPrunedDomain
//    instanceWithUpdatedValue
//  }
//
//  def solve(): List[Seq[Variable]] = {
//    ???
//    val emptyInstance = problem.emptyInstance
//    var solutionList = List[Seq[Variable]]()
//
//    @tailrec
//    def iterateSolution(solutionHistory: List[CSPInstance]): Unit = {
//      val currentSolution = solutionHistory.head
//      if (currentSolution.variables.forall(_.isDefined)) {
//        if (problem.satisfiesConstraints(currentSolution.variables)) {
//          solutionList = currentSolution.variables :: solutionList
//          return
//        }
//      }
//      val i = selectIndex(currentSolution)
//      val newValueOption = selectValue(currentSolution, i)
//      newValueOption match {
//        case Some(newValue) =>
//          val newSolution = getNewSolution(currentSolution, i, newValue)
//          if (problem.satisfiesWeakConstraints(newSolution.variables) && unassignedVariablesHaveLegalValues(newSolution)) {
//            iterateSolution(newSolution :: solutionHistory)
//          } else {
//            // remove this value from domain
//            val oldDomain = solutionHistory.head.domains(i)
//            val newDomain = oldDomain.filter(_ != newValue)
//            val solutionWithUpdatedDomain = solutionHistory.head.modify(_.domains.at(i)).setTo(newDomain)
//            iterateSolution(solutionHistory.modify(_.at(0)).setTo(solutionWithUpdatedDomain))
//          }
//        case None =>
//          // go back in history and remove checked variable from domain
//          val newHistory = solutionHistory.tail
//          if(newHistory.isEmpty) return
//          val solutionWithUpdatedDomain = newHistory.head.modify(_.domains.at(i)).setTo(Seq.empty) //???
//          iterateSolution(newHistory.modify(_.at(0)).setTo(solutionWithUpdatedDomain))
//      }
//    }
//    iterateSolution(List(emptyInstance))
//    solutionList
//  }
//}
case class ForwardCheckingSolver(problem: CSP) {
  def selectIndex(currentSolution: CSPInstance): Int = {
    val available = (currentSolution.variables zip currentSolution.domains).zipWithIndex.filter(_._1._1.isEmpty)
//    available.head._2
    available.minBy(_._1._2.size)._2
  }

  def unassignedVariablesHaveLegalValues(i: CSPInstance): Boolean = {
    val available = i.variables zip i.domains filter(_._1.isEmpty)
    available.forall(_._2.nonEmpty)
  }

  def selectValue(currentSolution: CSPInstance, i: Int) = {
    currentSolution.domains(i).head //TODO: use heuristics
//    currentSolution.domains(i).
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
      val i = selectIndex(currentSolution)
//      println(i)
//      println(currentSolution)

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

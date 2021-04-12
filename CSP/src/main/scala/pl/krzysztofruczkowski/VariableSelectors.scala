package pl.krzysztofruczkowski


abstract class VariableSelector() {
  def selectIndex(currentSolution: CSPInstance): Int
}

case class DefaultVariableSelector() extends VariableSelector() {
  override def selectIndex(currentSolution: CSPInstance): Int = {
    val available = (currentSolution.variables zip currentSolution.domains).zipWithIndex.filter(_._1._1.isEmpty)
    available.head._2
  }
}

case class MinDomainVariableSelector() extends VariableSelector() {
  override def selectIndex(currentSolution: CSPInstance): Int = {
    val available = (currentSolution.variables zip currentSolution.domains).zipWithIndex.filter(_._1._1.isEmpty)
    available.minBy(_._1._2.size)._2
  }
}

case class MaxDomainVariableSelector() extends VariableSelector() {
  override def selectIndex(currentSolution: CSPInstance): Int = {
    val available = (currentSolution.variables zip currentSolution.domains).zipWithIndex.filter(_._1._1.isEmpty)
    available.maxBy(_._1._2.size)._2
  }
}


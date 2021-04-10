package pl.krzysztofruczkowski

case class CSPInstance(variables: Seq[Variable], domains: Seq[Domain])
abstract class CSP() {
  //constraints that allow null values
  val weakConstraints: Seq[Constraint]

  //constraints that do not allow null values
  val constraints: Seq[Constraint]

  val emptyInstance: CSPInstance

  def satisfiesWeakConstraints(variables: Seq[Variable]) = weakConstraints.forall(p => p(variables))
  def satisfiesConstraints(variables: Seq[Variable]) = constraints.forall(p => p(variables))
}


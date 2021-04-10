package pl

package object krzysztofruczkowski {
  type Variable = Option[Any]
  type Domain = Seq[Any]
  type Constraint = Seq[Variable] => Boolean
}

package pl.krzysztofruczkowski

class MapColoringCSP(val connections: Map[Point,List[Point]], val k: Int) extends CSP() {
  val points = connections.keys.toList

  override val weakConstraints = Vector(
    variables => {
      connections.forall(c => {
        val point = c._1
        val otherPoints = c._2
        val firstColor = variables(points.indexOf(point))
        if (firstColor.isEmpty) true
        else {
          otherPoints.forall(otherPoint => {
            val secondColor = variables(points.indexOf(otherPoint))
            if (secondColor.isEmpty) true
            else firstColor != secondColor
          })
        }
      })
    })

  override val constraints = weakConstraints ++ Vector(variables => variables.forall(_.isDefined))

  override val emptyInstance = CSPInstance(
    variables = Vector.fill(points.size)(None),
    domains = Vector.fill(points.size)(1 to k)
  )
}


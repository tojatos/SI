package pl.krzysztofruczkowski

import com.softwaremill.quicklens._

case class BacktrackMapColoringSolver(mapColoringProblem: MapColoringProblem) {
  // k - number of colors allowed
  def solve(k: Int): List[MapColoringSolution] = {
    val pointList = mapColoringProblem.connections.keys.toList
    val emptySolution = MapColoringSolution.empty(pointList)
    var solutionList = List[MapColoringSolution]()

    def iterateSolution(i: Int, currentSolution: MapColoringSolution): Unit = {
      if (i >= pointList.size) {
        if(mapColoringProblem.satisfiesRequirements(currentSolution)) {
          solutionList = currentSolution :: solutionList
        }
        return
      }
      (0 until k).foreach { colorInt =>
        val newColorSolution = currentSolution.modify(_.pointsToColor.at(pointList(i))).setTo(Some(colorInt))
        if(mapColoringProblem.satisfiesWeakRequirements(newColorSolution)) {
          iterateSolution(i+1, newColorSolution)
        }
      }
    }
    iterateSolution(0, emptySolution)
    solutionList
  }
}

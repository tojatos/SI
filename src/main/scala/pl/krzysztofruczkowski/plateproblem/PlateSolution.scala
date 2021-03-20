package pl.krzysztofruczkowski.plateproblem

import pl.krzysztofruczkowski.plateproblem
import pl.krzysztofruczkowski.plateproblem.Direction.{Down, Left, Right, Up, isHorizontal}

import scala.util.Random

case class PlateSolution(paths: List[Path]) {
  def randomMutate(implicit random: Random): PlateSolution = {
    val randomPathIndex = random.nextInt(paths.length)
    val randomPath: Path = paths(randomPathIndex)

    val mutatedPath = randomPath.mutate(random)
    val newPaths = paths.updated(randomPathIndex, mutatedPath)

    PlateSolution(newPaths)
  }

  def cross(other: PlateSolution, random: Random): PlateSolution = {
//    val pathsToReplace = paths.length / 2
    val pathsToReplace = random.between(0, paths.length)
    val takeFromOriginalAtIndex = random.shuffle(List.fill(pathsToReplace)(false) ::: List.fill(paths.length - pathsToReplace)(true))
    val newPaths = paths.indices.map(i => if(takeFromOriginalAtIndex(i)) paths(i) else other.paths(i)).toList
    PlateSolution(newPaths)
  }
}

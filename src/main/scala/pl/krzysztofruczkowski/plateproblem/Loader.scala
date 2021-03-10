package pl.krzysztofruczkowski.plateproblem

import scala.io.Source
import scala.language.postfixOps

object Loader {
  def loadProblems(filenames: Seq[String]): Seq[PlateProblem] =
    filenames map ( Source.fromResource(_).getLines() ) map (PlateProblem deserialize)
}

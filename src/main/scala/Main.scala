import scala.language.postfixOps
import Direction._

object Main extends App {
  val filenames = 0 to 3 map ( x => s"zad$x.txt" )
  val problems = Loader.loadProblems(filenames)
  problems foreach println
  val first_problem = problems.head
  println(first_problem.fitness(PlateSolution(List(Path(List(Segment(Right, 4))), Path(List(Segment(Up, 2)))))))
}

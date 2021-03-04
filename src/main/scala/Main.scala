import scala.io.Source
import scala.language.postfixOps

object Main extends App {
  val filenames = 0 to 3 map ( x => s"zad$x.txt" )
  val problems = filenames map ( Source fromResource _ getLines ) map (PlateProblem deserialize)
  problems foreach println
}

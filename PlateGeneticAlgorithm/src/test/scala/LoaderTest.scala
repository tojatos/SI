import org.scalatest.FunSuite
import pl.krzysztofruczkowski.plateproblem.Loader

class LoaderTest extends FunSuite {
  test("Loader.loadProblems") {
    val filenames = 0 to 3 map ( x => s"zad$x.txt" )
    val problems = Loader.loadProblems(filenames)
    assert(problems.length === 4)
  }
}

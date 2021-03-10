package pl.krzysztofruczkowski

import pl.krzysztofruczkowski.plateproblem.{Loader, PlateProblem}
import pl.krzysztofruczkowski.ui.{SceneManager, Scenes}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage

object Main extends JFXApp {
  val filenames: Seq[String] = 0 to 3 map (x => s"zad$x.txt" )
  val problems: Seq[PlateProblem] = Loader.loadProblems(filenames)
  stage = new PrimaryStage {
        title = "Plate problem analyser"
  }
//  val po = new RandomPlateProblemOptimizer(problems.head)
//  for (_ <- 1 to 1000) {
//    po.iterate()
//  }
  SceneManager.loadScene(Scenes.Main)
}
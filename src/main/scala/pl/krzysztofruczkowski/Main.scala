package pl.krzysztofruczkowski

import pl.krzysztofruczkowski.ui.{SceneManager, Scenes}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage

object Main extends JFXApp {
  val filenames: Seq[String] = 0 to 3 map (x => s"zad$x.txt" )
  val problems: Seq[PlateProblem] = Loader.loadProblems(filenames)
  stage = new PrimaryStage {
        title = "Plate problem analyser"
  }
  SceneManager.loadScene(Scenes.Main)
}
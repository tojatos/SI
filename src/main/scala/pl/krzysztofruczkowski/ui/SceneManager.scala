package pl.krzysztofruczkowski.ui

import javafx.scene.Parent
import pl.krzysztofruczkowski.Main
import scalafx.scene.Scene
import scalafx.Includes._
import scalafxml.core.{FXMLView, NoDependencyResolver}

object Scenes {
  val Main = "/main.fxml"
  val Problem = "/problem.fxml"
}

object SceneManager {
  def loadScene(resourcePath: String): Unit = {
      val resource = getClass.getResource(resourcePath)
      val root: Parent = FXMLView(resource, NoDependencyResolver)
      Main.stage.scene = new Scene(root)
  }
}


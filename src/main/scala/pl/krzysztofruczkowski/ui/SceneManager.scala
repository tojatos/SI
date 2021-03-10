package pl.krzysztofruczkowski.ui

import javafx.scene.Parent
import pl.krzysztofruczkowski.Main
import scalafx.scene.Scene
import scalafx.Includes._
import scalafxml.core.{FXMLView, NoDependencyResolver}

object SceneManager {
  def loadScene(resourcePath: String): Unit = {
      val resource = getClass.getResource(resourcePath)
      val root: Parent = FXMLView(resource, NoDependencyResolver)
      Main.stage.scene = new Scene(root)
  }
}


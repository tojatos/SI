import java.net.URL
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.Includes._
import scalafxml.core.{FXMLView, NoDependencyResolver}

import scala.language.postfixOps

object Main extends JFXApp {
  val filenames: Seq[String] = 0 to 3 map (x => s"zad$x.txt" )
  val problems: Seq[PlateProblem] = Loader.loadProblems(filenames)
  val resource: URL = getClass.getResource("main.fxml")
  val root = FXMLView(resource, NoDependencyResolver)
  stage = new PrimaryStage {
        title = "Plate problem analyser"
        scene = new Scene(root)
  }

}

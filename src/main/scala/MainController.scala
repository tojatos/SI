import javafx.event.ActionEvent
import scalafx.scene.control.{Button, Control}
import scalafx.scene.layout.{Region, VBox}
import scalafxml.core.macros.sfxml

@sfxml
class MainController(val problemsVbox: VBox) {
  val filenames: Seq[String] = 0 to 3 map (x => s"zad$x.txt" )
  val problems: Seq[PlateProblem] = Loader.loadProblems(filenames)

  val buttons: Seq[Button] = filenames.zip(problems).map(t => new Button {
    text = t._1
    onAction = (_: ActionEvent) => println(t._2)
    prefWidth = 400
    prefHeight = 60
    maxWidth = Double.MaxValue
  })
  buttons.foreach(b => problemsVbox.children.add(b))
//  problemsVbox.spacing = 5
//  problemsVbox.fillWidth = true
}

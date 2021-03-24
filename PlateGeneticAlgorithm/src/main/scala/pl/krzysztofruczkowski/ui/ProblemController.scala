package pl.krzysztofruczkowski.ui

import pl.krzysztofruczkowski.plateproblem.Direction._
import pl.krzysztofruczkowski.plateproblem._
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.control.Button
import scalafx.scene.paint.Color._
import scalafxml.core.macros.sfxml

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.Random

@sfxml
class ProblemController(val plateCanvas: Canvas, val iterButton: Button) {
  implicit val context: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  val selectedProblem: PlateProblem = StaticData.selectedProblem.get
  val unit = 30
  val margin = 300
  val baseCircleUnit = 3
  val bigCircleUnit = 11
  val (w, h) = (selectedProblem.width - 1, selectedProblem.height - 1)
  val allPoints = for (a_ <- 0 to w; b_ <- 0 to h) yield (a_, b_)
  val allCanvasPoints = allPoints.map(p => (p._1 * unit + margin / 2, p._2 * unit + margin / 2))
  val pointToCanvasPointMap = allPoints zip allCanvasPoints.map(x => (x._1.toDouble, x._2.toDouble)) toMap
  val colors = List(Red, DarkOrange, Violet, Black, White, Cyan, Yellow, Brown, Chocolate, Wheat, Aqua)

  val infiniteColors = LazyList.continually(colors).flatten

  plateCanvas.width = w * unit + margin
  plateCanvas.height = h * unit + margin

  val gc: GraphicsContext = plateCanvas.graphicsContext2D

  def reset(): Unit = {
    drawProblem(selectedProblem)
  }
  implicit val random: Random = new Random()
  lazy val po = new GeneticPlateProblemOptimizer(selectedProblem, GeneticPlateProblemParameters())
  def drawBestSolution(): Unit = {
    val filenames: Seq[String] = 0 to 3 map (x => s"zad$x.txt" )
    val problems: Seq[PlateProblem] = Loader.loadProblems(filenames).drop(1)
    val index = problems.indexOf(selectedProblem)
    val bestSolutions = List(
      PlateSolution(List(Path(List(Segment(Down,2), Segment(Right,7), Segment(Up,2))), Path(List(Segment(Up,1), Segment(Right,4), Segment(Down,3))), Path(List(Segment(Left,3), Segment(Up,9), Segment(Right,4))), Path(List(Segment(Down,1), Segment(Right,9), Segment(Up,11), Segment(Left,4))), Path(List(Segment(Up,2))), Path(List(Segment(Up,1), Segment(Right,6), Segment(Down,1))), Path(List(Segment(Up,1), Segment(Left,8), Segment(Up,12), Segment(Right,8))), Path(List(Segment(Down,4), Segment(Right,3))))),
      PlateSolution(List(Path(List(Segment(Up,7), Segment(Right,6), Segment(Down,14), Segment(Right,3), Segment(Up,14), Segment(Right,4), Segment(Down,14), Segment(Right,6), Segment(Up,7))), Path(List(Segment(Up,16))), Path(List(Segment(Up,15))), Path(List(Segment(Up,16))), Path(List(Segment(Up,15))))),
      PlateSolution(List(Path(List(Segment(Up,1), Segment(Right,13), Segment(Down,1), Segment(Right,4))), Path(List(Segment(Down,1), Segment(Right,19), Segment(Up,1))), Path(List(Segment(Left,1), Segment(Down,4), Segment(Right,19), Segment(Up,4), Segment(Left,1))), Path(List(Segment(Right,8), Segment(Up,1), Segment(Right,11), Segment(Down,1))), Path(List(Segment(Down,1), Segment(Right,14), Segment(Up,1), Segment(Right,3))), Path(List(Segment(Right,5), Segment(Up,1), Segment(Right,14), Segment(Down,1))), Path(List(Segment(Down,1), Segment(Right,9), Segment(Up,1), Segment(Right,8))), Path(List(Segment(Up,1), Segment(Right,19), Segment(Down,1))), Path(List(Segment(Right,17))), Path(List(Segment(Up,1), Segment(Right,19), Segment(Down,1))))),
    )

    drawSolution(bestSolutions(index))
  }
  Future {
    reset()
    drawSolution(po.getBest().plateSolution)
//    drawBestSolution()
  }

  def drawProblem(problem: PlateProblem): Unit = {
    // fill with green
    gc.fill = Green
    gc.fillRect(0, 0, plateCanvas.width.value, plateCanvas.height.value)

    // draw pins
    gc.stroke = Black
    gc.lineWidth = 1
    allCanvasPoints.foreach(t =>
      gc.strokeOval(t._1, t._2, baseCircleUnit, baseCircleUnit)
    )

    problem.pairs zip infiniteColors foreach(x => {
      val (points, color) = x
      val (p1, p2) = (points._1.toTuple, points._2.toTuple)
      val cp1 = pointToCanvasPointMap(p1)
      val cp2 = pointToCanvasPointMap(p2)

      gc.fill = color
      gc.fillOval(cp1._1 - bigCircleUnit / 2, cp1._2 - bigCircleUnit / 2, bigCircleUnit, bigCircleUnit)
      gc.fillOval(cp2._1 - bigCircleUnit / 2, cp2._2 - bigCircleUnit / 2, bigCircleUnit, bigCircleUnit)
    })
  }

  def drawSolution(plateSolution: PlateSolution): Unit = {
    (selectedProblem.pairs map(_._1) lazyZip plateSolution.paths lazyZip infiniteColors).toList.foreach { args =>
      val (startPoint, path, color) = args
      var (x, y) = pointToCanvasPointMap(startPoint.x, startPoint.y)
      gc.fill = color
      gc.lineWidth = 4
      gc.stroke = color
      gc.beginPath()
      gc.moveTo(x, y)
      path.segments.foreach { a =>
        a match {
          case Segment(Right, value) => x += value * unit
          case Segment(Left, value) => x -= value * unit
          case Segment(Up, value) => y += value * unit
          case Segment(Down, value) => y -= value * unit
        }
        gc.lineTo(x, y)
      }
      gc.strokePath()
    }
  }

  val iterAction = () => {
      Future {
        for (_ <- 1 to Const.PROBLEM_CONTROLLER_NUMBER_OF_ITERATIONS) {
          po.iterate()
        }
        reset()
        drawSolution(po.getBest().plateSolution)
        println("iteration done")
      }
    }
  val randAction = () => {
    Future {
      reset()
      drawSolution(selectedProblem.generateRandomSolution(random))
    }
  }

  val infiniteIterAction = () => {
    Future {
//      while (!selectedProblem.isValid(po.getBest().plateSolution)) {
      for(i <- LazyList.from(1)) {
        println(s"Gen $i")
        po.iterate()
        if(i % Const.PROBLEM_CONTROLLER_REDRAW_EVERY == 0) {
          reset()
          drawSolution(po.getBest().plateSolution)
        }
      }
    }
  }


  iterButton.onAction = _ => infiniteIterAction()
}
